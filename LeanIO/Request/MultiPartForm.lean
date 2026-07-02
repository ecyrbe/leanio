import Lean
import Std.Async.ContextAsync
import LeanIO.Request.FromRequestBody
import LeanIO.Utils

namespace LeanIO
open Std.Http Std.Async

private inductive Phase : Type where
  | ready
  | inFile
  | done

private instance : Inhabited Phase where
  default := .ready

private structure MultipartInner : Type where
  buf        : ByteArray
  pos        : Nat
  boundStart : ByteArray
  boundSepSearch : Search ByteArray
  stream     : Body.Stream
  phase      : Phase

structure FormFile where
  name        : String
  filename    : String
  contentType : String
  inner       : IO.Ref MultipartInner

inductive MultipartEntry : Type where
  | field (name : String) (value : String)
  | file  (file : FormFile)

structure MultiPartForm where
  inner : IO.Ref MultipartInner

abbrev crlf : ByteArray := "\r\n".toUTF8
abbrev crlfcrlf : ByteArray := "\r\n\r\n".toUTF8
abbrev endMarker : ByteArray := "--\r\n".toUTF8
abbrev quote: ByteArray := "\"".toUTF8
abbrev namePat : ByteArray := "name=\"".toUTF8
abbrev fileNamePat : ByteArray := "filename=\"".toUTF8
abbrev contentTypePat : ByteArray := "Content-Type: ".toUTF8

abbrev crlfcrlfSearch := Search.new crlfcrlf
abbrev quoteSearch := Search.new quote
abbrev crlfSearch := Search.new crlf
abbrev nameSearch := Search.new namePat
abbrev fileNameSearch := Search.new fileNamePat
abbrev contentTypeSearch := Search.new contentTypePat


private def startsWith (haystack : ByteArray) (needle : ByteArray) (pos : Nat := 0) : Bool :=
  if pos + needle.size > haystack.size then false
  else Id.run do
    let mut hi := haystack.iter.forward pos
    let mut ni := needle.iter
    while !ni.atEnd do
      if hi.curr ≠ ni.curr then return false
      hi := hi.next
      ni := ni.next
    return true

private def joinChunks (chunks : List ByteArray) : ByteArray :=
  chunks.reverse.foldl (fun acc b => acc ++ b) ByteArray.empty

private def scanDelim (data : ByteArray) (delimSearch: Search ByteArray) : Option (ByteArray × ByteArray) :=
  match delimSearch.search data 0 with
  | some dpos => some (data.extract 0 dpos, data.extract (dpos + delimSearch.needle.size) data.size)
  | none => none

private def splitOverlap (data : ByteArray) (delimSearch: Search ByteArray) : Option (ByteArray × ByteArray) :=
  let overlap := min data.size (delimSearch.needle.size - 1)
  if data.size > overlap then
    some (data.extract 0 (data.size - overlap), data.extract (data.size - overlap) data.size)
  else none

private def readMore (inner : IO.Ref MultipartInner) : ContextAsync Bool := do
  let st ← inner.get
  if st.pos ≥ 65536 then
    inner.modify fun s => { s with buf := s.buf.extract s.pos s.buf.size, pos := 0 }
  let st ← inner.get
  let chunkOpt ← st.stream.recv
  match chunkOpt with
  | none => return false
  | some chunk =>
    inner.modify fun s => { s with buf := s.buf ++ chunk.data }
    return true

private partial def readUntilGo (inner : IO.Ref MultipartInner) (delimSearch : Search ByteArray) (chunks : List ByteArray) : ContextAsync (List ByteArray) := do
  let st ← inner.get
  let view := st.buf.extract st.pos st.buf.size
  match scanDelim view delimSearch with
  | some (body, _) =>
    inner.modify fun s => { s with pos := st.pos + body.size + delimSearch.needle.size }
    return body :: chunks
  | none =>
    if view.size = 0 then
      let ok ← readMore inner
      if ok then readUntilGo inner delimSearch chunks
      else return chunks
    else
      match splitOverlap view delimSearch with
      | some (chunk, _) =>
        inner.modify fun s => { s with pos := st.pos + chunk.size }
        readUntilGo inner delimSearch (chunk :: chunks)
      | none =>
        if ← readMore inner then
          readUntilGo inner delimSearch chunks
        else
          let st' ← inner.get
          inner.modify fun s => { s with pos := s.buf.size }
          return st'.buf.extract st'.pos st'.buf.size :: chunks

private def readUntil (inner : IO.Ref MultipartInner) (delimSearch : Search ByteArray) : ContextAsync ByteArray := do
  let cs ← readUntilGo inner delimSearch []
  return joinChunks cs

private partial def skip (inner : IO.Ref MultipartInner) (pref : ByteArray) : ContextAsync Unit := do
  let st ← inner.get
  if startsWith st.buf pref st.pos then
    inner.modify fun s => { s with pos := s.pos + pref.size }
  else
    let ok ← readMore inner
    if ok then skip inner pref

private def extractQuotedValue (ba : ByteArray) (keyPrefixSearch : Search ByteArray) : Option ByteArray :=
  match keyPrefixSearch.search ba with
  | none => none
  | some pos =>
    let afterKey := ba.extract (pos + keyPrefixSearch.needle.size) ba.size
    match quoteSearch.search afterKey with
    | none => none
    | some endPos => some (afterKey.extract 0 endPos)

private def extractHeaderValue (ba : ByteArray) (nameSearch : Search ByteArray) : Option ByteArray :=
  match nameSearch.search ba with
  | none => none
  | some pos =>
    let afterName := ba.extract (pos + nameSearch.needle.size) ba.size
    match crlfSearch.search afterName with
    | none => some afterName
    | some endPos => some (afterName.extract 0 endPos)

private def parseHeaders (hdrBytes : ByteArray) : Option String × Option String × String :=
  let nameRaw := extractQuotedValue hdrBytes nameSearch
  let fnameRaw := extractQuotedValue hdrBytes fileNameSearch
  let ctRaw := extractHeaderValue hdrBytes contentTypeSearch
  let name := nameRaw >>= String.fromUTF8?
  let filename := fnameRaw >>= String.fromUTF8?
  let contentType := match ctRaw with
    | some ba =>
      let s := (String.fromUTF8? ba).getD ""
      s.replace "\r" ""
    | none => "application/octet-stream"
  (name, filename, contentType)

private def consumeFileBody (inner : IO.Ref MultipartInner) : ContextAsync ByteArray := do
  let st ← inner.get
  let body ← readUntil inner st.boundSepSearch
  inner.modify fun s => { s with phase := .ready }
  return body

private def parseNextEntry (inner : IO.Ref MultipartInner) : ContextAsync (Option MultipartEntry) := do
  let st ← inner.get
  let first := st.pos = 0 && st.buf.size = 0
  if first then
    skip inner st.boundStart
  skip inner crlf
  let st ← inner.get
  if startsWith st.buf endMarker st.pos then
    inner.modify fun s => { s with pos := s.pos + endMarker.size, phase := .done }
    return none
  let headerBytes ← readUntil inner crlfcrlfSearch
  let (name, filename, contentType) := parseHeaders headerBytes
  match name with
    | none =>
        inner.modify fun s => { s with phase := .done }
        return none
    | some name =>
        match filename with
        | none =>
          let bodyBytes ← readUntil inner st.boundSepSearch
          return some (.field name ((String.fromUTF8? bodyBytes).getD ""))
        | some fn =>
          inner.modify fun s => { s with phase := .inFile }
          return some (.file {
            name := name, filename := fn, contentType := contentType, inner := inner
          })

private partial def streamFile (inner : IO.Ref MultipartInner) (cb : ByteArray → ContextAsync Unit) (tails : List ByteArray) : ContextAsync Unit := do
  let st ← inner.get
  let remaining := st.buf.size - st.pos
  if remaining > 0 then
    inner.modify fun s => { s with pos := s.buf.size }
    streamFile inner cb (st.buf.extract st.pos st.buf.size :: tails)
    return
  let chunkOpt ← st.stream.recv
  match chunkOpt with
  | none =>
    let pre := joinChunks tails
    if pre.size > 0 then cb pre
    inner.modify fun s => { s with phase := .ready }
  | some chunk =>
    let data := joinChunks tails ++ chunk.data
    match scanDelim data st.boundSepSearch with
    | some (body, rest) =>
      inner.modify fun s => { s with buf := rest, pos := 0, phase := .ready }
      cb body
    | none =>
      match splitOverlap data st.boundSepSearch with
      | some (body, tail) =>
        cb body
        streamFile inner cb (tail :: [])
      | none =>
        streamFile inner cb (data :: [])

private def extractBoundary (contentType : String) : Option String :=
  let parts := contentType.split (fun c => c == ';')
  let rec find (ps : List String.Slice) : Option String :=
    match ps with
    | [] => none
    | p :: rest =>
      let s := p.toString.trimAscii
      if s.startsWith "boundary=" then
        let val := s.dropPrefix "boundary="
        let val := val.takeWhile (fun c => c ≠ ';' && c ≠ ' ' && c ≠ '\"') |>.toString
        some val
      else find rest
  find parts.toList

def MultiPartForm.nextEntry (mp : MultiPartForm) : ContextAsync (Option MultipartEntry) := do
  let st ← mp.inner.get
  match st.phase with
  | .done => return none
  | .inFile => return none
  | .ready => parseNextEntry mp.inner

def FormFile.stream (f : FormFile) (cb : ByteArray → ContextAsync Unit) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile => streamFile f.inner cb []
  | _ => return

def FormFile.discard (f : FormFile) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile =>
    let _ ← consumeFileBody f.inner
  | _ => return

def FormFile.save (f : FormFile) (path : System.FilePath) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile =>
    let handle ← IO.FS.Handle.mk path .write
    f.stream fun chunk => do handle.write chunk
    handle.flush
  | _ => return

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
      { buf := ByteArray.empty, pos := 0
        boundStart := boundSep.extract 2 boundSep.size
        boundSepSearch := Search.new boundSep
        stream := req.body, phase := .ready }
    let ref ← IO.mkRef innerVal
    return .ok { inner := ref }

end LeanIO
