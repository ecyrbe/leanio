import Lean
import Std.Async.ContextAsync
import Std.Data.ByteSlice
import LeanIO.Request.FromRequestBody
import LeanIO.Utils
import LeanIO.Data.ChunkBuffer
import LeanIO.Data.MimeType


namespace LeanIO
open Std.Http Std.Async Std.Slice MimeType

private inductive Phase : Type where
  | ready
  | inFile
  | done

private instance : Inhabited Phase where
  default := .ready

private structure MultipartInner : Type where
  cb             : ChunkBuffer
  pos            : Nat
  boundStart     : ByteArray
  boundSepSearch : Search ChunkBuffer
  stream         : Body.Stream
  phase          : Phase
  nameCounter    : Nat

structure FormFile where
  name        : String
  filename    : String
  contentType : String
  headers     : Headers
  inner       : IO.Ref MultipartInner

inductive MultipartEntry : Type where
  | field (name : String) (value : String)
  | file  (file : FormFile)

structure MultiPartForm where
  inner : IO.Ref MultipartInner

abbrev crlf : ByteArray := "\r\n".toUTF8
abbrev endMarker : ByteArray := "--\r\n".toUTF8

open Lean Elab Term in
elab "kmp! " t:str : term => do
      let s := t.getString
      let cb := ChunkBuffer.ofByteArray s.toUTF8
      let lps := (Search.new cb).LPS
      let lpsNums : Array (TSyntax `term) := lps.map (λ (n : Nat) => ⟨Syntax.mkNumLit (toString n)⟩)
      let stx ← `(Search.mk (α := ChunkBuffer) (ChunkBuffer.ofByteArray ($(quote s) : String).toUTF8) #[$[$lpsNums],*])
      elabTerm stx none

abbrev crlfcrlfSearch := kmp! "\r\n\r\n"
abbrev crlfSearch     := kmp! "\r\n"

private abbrev contentDisposition : Header.Name := .ofString! "content-disposition"

/-- Emit each slice of `body` to `cb`, converting via `toByteArrayFast`. -/
@[inline]
private def emitChunks (body : ChunkBuffer) (cb : ByteArray → ContextAsync Unit) : ContextAsync Unit := do
    for chunk in body.chunks do
      cb chunk.toByteArrayFast

private def readMore (inner : IO.Ref MultipartInner) : ContextAsync Bool := do
  let st ← inner.get
  if st.pos ≥ 65536 then
    inner.modify fun s => { s with cb := st.cb.extract st.pos st.cb.size, pos := 0 }
  let st ← inner.get
  match ← st.stream.recv with
  | none => return false
  | some chunk =>
    inner.modify fun s => { s with cb := s.cb.add chunk.data }
    return true

private partial def readUntilGo (inner : IO.Ref MultipartInner) (delimSearch : Search ChunkBuffer) (startPos : Nat) : ContextAsync (ChunkBuffer × Nat) := do
  let st ← inner.get
  match st.cb.searchSafe delimSearch st.pos with
  | .found dpos =>
    let body := st.cb.extract st.pos dpos
    let newPos := dpos + delimSearch.needle.size
    inner.modify fun s => { s with pos := newPos }
    return (body, newPos)
  | .notFound safe =>
    inner.modify fun s => { s with pos := st.pos + safe }
    readUntilGo inner delimSearch startPos
  | .needMore =>
    if ← readMore inner then
      readUntilGo inner delimSearch startPos
    else
      let total := st.cb.size
      let body := st.cb.extract startPos total
      inner.modify fun s => { s with pos := total }
      return (body, total)

private def readUntil (inner : IO.Ref MultipartInner) (delimSearch : Search ChunkBuffer) : ContextAsync ByteArray := do
  let st ← inner.get
  let (body, _) ← readUntilGo inner delimSearch st.pos
  return body.toByteArray

private partial def skip (inner : IO.Ref MultipartInner) (pref : ByteArray) : ContextAsync Unit := do
  let st ← inner.get
  if st.cb.startsWithAt st.pos pref then
    inner.modify fun s => { s with pos := s.pos + pref.size }
  else
    let ok ← readMore inner
    if ok then skip inner pref

/-- Parse a header line "Name: value" into a `(Name, Value)` pair. Splits on first colon only. -/
private def parseOneHeader (line : String) : Option (Header.Name × Header.Value) :=
  match String.splitOnce line ':' with
  | none => none
  | some (name, val) =>
    let name := name.trimAscii.toString
    let val := val.trimAscii.toString
    (Header.Name.ofString? name).bind fun n =>
    (Header.Value.ofString? val).map fun v => (n, v)

/-- Parse raw header bytes into `Std.Http.Headers`. Returns `none` on parse failure. -/
private def parseHeaders (hdrBytes : ByteArray) : Option Headers := do
  let hdrStr ← String.fromUTF8? hdrBytes
  (hdrStr.splitOn "\r\n").foldlM (fun (hds : Headers) (line : String) =>
    let clean := line.trimAscii.toString
    if clean.isEmpty then some hds else
      match parseOneHeader clean with
      | some (n, v) => some (hds.insert n v)
      | none => none
    ) (∅ : Headers)

/-- Extract a quoted parameter value from a Content-Disposition value string. -/
private def extractParam (params : String) (key : String) : Option String :=
  let keySuffix := key ++ "=\""
  (params.split (· == ';')).toList.findSome? fun part =>
    let trimmed := part.toString.trimAscii.toString
    if trimmed.startsWith keySuffix then
      let inner := trimmed.drop keySuffix.length
      inner.takeWhile (· ≠ '\"') |> toString |> fun s =>
        if s.isEmpty then none else some s
    else none

/-- Extract the `name` parameter from Content-Disposition headers. -/
private def contentDispositionName (hds : Headers) : Option String :=
  match hds.get? contentDisposition with
  | none => none
  | some v => extractParam v.value "name"

/-- Extract the `filename` parameter from Content-Disposition headers. -/
private def contentDispositionFilename (hds : Headers) : Option String :=
  match hds.get? contentDisposition with
  | none => none
  | some v => extractParam v.value "filename"

/-- Extract the Content-Type from headers, defaulting to `text/plain` per RFC 2046 §5.1. -/
private def headerContentType (hds : Headers) : String :=
  match hds.get? .contentType with
  | some v => v.value
  | none => "text/plain"

/-- Build a generated filename: `{name}{counter}.{ext}`. -/
private def generatedFilename (name : String) (counter : Nat) (mime : String) : String :=
  s!"{name}{counter}.{extForMime mime}"

private def parseNextEntry (inner : IO.Ref MultipartInner) : ContextAsync (Option MultipartEntry) := do
  let st ← inner.get
  if st.pos = 0 && st.cb.size = 0 then
    skip inner st.boundStart
  skip inner crlf
  let st ← inner.get
  if st.cb.startsWithAt st.pos endMarker then
    inner.modify fun s => { s with pos := s.pos + endMarker.size, phase := .done }
    return none
  let headerBytes ← readUntil inner crlfcrlfSearch
  let some hds := parseHeaders headerBytes
    | inner.modify fun s => { s with phase := .done }
      return none
  let name := contentDispositionName hds
  let filename := contentDispositionFilename hds
  let contentType := headerContentType hds
  match name with
    | none =>
        inner.modify fun s => { s with phase := .done }
        return none
    | some name =>
        match filename with
        | none =>
          if contentType.startsWith "text/" then
            let bodyBytes ← readUntil inner st.boundSepSearch
            return some (.field name ((String.fromUTF8? bodyBytes).getD ""))
          else
            let counter := st.nameCounter
            inner.modify fun s => { s with nameCounter := s.nameCounter + 1, phase := .inFile }
            let fn := generatedFilename name counter contentType
            return some (.file {
              name := name, filename := fn, contentType := contentType, headers := hds, inner := inner
            })
        | some fn =>
          inner.modify fun s => { s with phase := .inFile }
          return some (.file {
            name := name, filename := fn, contentType := contentType, headers := hds, inner := inner
          })

/-- Split `dataCB` at `boundSearch`. If found: emit body slices, store rest. Otherwise:
    emit safe portion and pass tail/remainder to `k`. -/
private def emitBoundary (inner : IO.Ref MultipartInner) (boundSearch : Search ChunkBuffer)
    (dataCB : ChunkBuffer) (cb : ByteArray → ContextAsync Unit)
    (k : ChunkBuffer → ContextAsync Unit) : ContextAsync Unit := do
  let dataLen := dataCB.size
  match dataCB.searchSafe boundSearch 0 with
  | .found dpos =>
    let needleSz := boundSearch.needle.size
    emitChunks (dataCB.extract 0 dpos) cb
    inner.modify fun s => { s with cb := dataCB.extract (dpos + needleSz) dataLen, pos := 0, phase := .ready }
  | .notFound safe =>
    emitChunks (dataCB.extract 0 safe) cb
    k (dataCB.extract safe dataLen)
  | .needMore =>
    k dataCB

/-- Stream raw chunks from the stream. No buffering in ChunkBuffer. -/
private partial def streamRaw (inner : IO.Ref MultipartInner) (boundSearch : Search ChunkBuffer)
    (cb : ByteArray → ContextAsync Unit) (carry : ChunkBuffer) : ContextAsync Unit := do
  let st ← inner.get
  let chunkOpt ← st.stream.recv
  match chunkOpt with
  | none =>
    if carry.size > 0 then
      emitBoundary inner boundSearch carry cb fun rest => do
        emitChunks rest cb
    inner.modify fun s => { s with phase := .ready }
  | some chunk =>
    let dataLen := carry.size + chunk.data.size
    emitBoundary inner boundSearch (carry.add chunk.data) cb fun rest => do
      if rest.size = dataLen then
        emitChunks rest cb
        streamRaw inner boundSearch cb ChunkBuffer.empty
      else
        streamRaw inner boundSearch cb rest

private def extractBoundary (contentType : String) : Option String :=
  (contentType.split (· = ';')).toList.filterMap (fun s =>
    (s.toString.trimAscii.dropPrefix? "boundary=").map fun val =>
      val.takeWhile (fun c => c ≠ ';' && c ≠ ' ' && c ≠ '\"') |>.toString)
  |>.head?

private def startStreamFile (f : FormFile) (cb : ByteArray → ContextAsync Unit) : ContextAsync Unit := do
  let st ← f.inner.get
  f.inner.set { st with cb := ChunkBuffer.empty, pos := 0 }
  streamRaw f.inner st.boundSepSearch cb (st.cb.extract st.pos st.cb.size)

/-
 Public API
-/

/-!
Grammar for multipart/<any> content type:
multipart-body  := preamble 1*encapsulation close-delimiter epilogue
encapsulation   := delimiter body-part CRLF
delimiter       := "--" boundary CRLF
close-delimiter := "--" boundary "--" CRLF
preamble        := discard-text
epilogue        := discard-text
discard-text    := *(*text CRLF)
body-part       := *(header-field CRLF) CRLF [body-content]
-/

def MultiPartForm.nextEntry (mp : MultiPartForm) : ContextAsync (Option MultipartEntry) := do
  let st ← mp.inner.get
  match st.phase with
  | .done => return none
  | .inFile => return none
  | .ready => parseNextEntry mp.inner

def FormFile.stream (f : FormFile) (cb : ByteArray → ContextAsync Unit) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile => startStreamFile f cb
  | _ => return

def FormFile.discard (f : FormFile) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile =>
    f.inner.modify fun s => { s with cb := ChunkBuffer.empty, pos := 0 }
    streamRaw f.inner st.boundSepSearch (fun _ => pure ()) ChunkBuffer.empty
  | _ => return

def FormFile.save (f : FormFile) (path : System.FilePath) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile =>
    let handle ← IO.FS.Handle.mk path .write
    let fStream := IO.FS.Stream.ofHandle handle
    f.stream fun chunk => do fStream.write chunk
    fStream.flush
  | _ => return

instance : HasMimeTypes (MultiPartForm) where
  mimes? := some [MimeType.multipartForm]

instance : FromRequestBody MultiPartForm where
  from_request_body req := do
    let ctOpt := req.line.headers.get? .contentType
    let ctStr : String :=
      match ctOpt with
      | none => ""
      | some hv => toString hv
    if !ctStr.startsWith "multipart/form-data" then
      return .error s!"multipart/form-data content-type expected, received: {ctStr}"
    let some boundary := extractBoundary ctStr
      | return .error "failed to extract boundary from content-type"
    let boundSep := ("\r\n--" ++ boundary).toUTF8
    let innerVal : MultipartInner :=
      { cb := ChunkBuffer.empty, pos := 0
        boundStart := boundSep.extract 2 boundSep.size
        boundSepSearch := Search.new (ChunkBuffer.ofByteArray boundSep)
        stream := req.body, phase := .ready, nameCounter := 0 }
    let ref ← IO.mkRef innerVal
    return .ok { inner := ref }

end LeanIO
