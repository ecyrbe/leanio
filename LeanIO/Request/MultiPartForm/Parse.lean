import Std.Async.ContextAsync
import LeanIO.Data.MimeType
import LeanIO.Request.MultiPartForm.Defs
import LeanIO.Request.MultiPartForm.Headers
import LeanIO.Request.MultiPartForm.Stream

namespace LeanIO
open Std.Http Std.Async Std.Slice MimeType
open Lean Elab Term

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


elab "kmp! " t:str : term => do
      let s := t.getString
      let cb := ChunkBuffer.ofByteArray s.toUTF8
      let lps := (Search.new cb).LPS
      let lpsNums : Array (TSyntax `term) := lps.map (λ (n : Nat) => ⟨Syntax.mkNumLit (toString n)⟩)
      let stx ← `(Search.mk (α := ChunkBuffer) (ChunkBuffer.ofByteArray ($(quote s) : String).toUTF8) #[$[$lpsNums],*])
      elabTerm stx none

abbrev crlfcrlfSearch := kmp! "\r\n\r\n"
abbrev crlf : ByteArray := "\r\n".toUTF8
abbrev endMarker : ByteArray := "--\r\n".toUTF8

def extractBoundary (contentType : String) : Option String :=
  (contentType.split (· = ';')).toList.filterMap (fun s =>
    (s.toString.trimAscii.dropPrefix? "boundary=").map fun val =>
      val.takeWhile (fun c => c ≠ ';' && c ≠ ' ' && c ≠ '\"') |>.toString)
  |>.head?

/-- Build a generated filename: `{name}{counter}.{ext}`. -/
def generatedFilename (name : String) (counter : Nat) (mime : String) : String :=
  s!"{name}{counter}.{extForMime mime}"


/-- Transition to `.inFile` and return a file entry. -/
@[inline]
private def fileEntry (inner : IO.Ref MultipartInner) (name fn : String) (contentType : String) (hds : Headers) : ContextAsync MultipartEntry := do
  inner.modify fun s => { s with phase := .inFile }
  return .file { name, filename := fn, contentType, headers := hds, inner }

/-- Transition to `.done` and stop. -/
@[inline]
private def stop (inner : IO.Ref MultipartInner) : ContextAsync (Option MultipartEntry) := do
  inner.modify fun s => { s with phase := .done }
  return none

/--
Parse the next multipart entry from the stream.

1. Skip preamble (first boundary + CRLF)
2. Check for close-delimiter
3. Parse headers
4. text/* without filename → field; otherwise → file
-/
def parseNextEntry (inner : IO.Ref MultipartInner) : ContextAsync (Option MultipartEntry) := do
  let st ← inner.get
  if st.pos = 0 && st.cb.size = 0 then
    skip inner st.boundStart
  skip inner crlf

  let st ← inner.get
  if st.cb.startsWithAt st.pos endMarker then
    inner.modify fun s => { s with pos := s.pos + endMarker.size }
    return ← stop inner

  let headerBytes ← readUntil inner crlfcrlfSearch
  let some hds := parseHeaders headerBytes | return ← stop inner
  let some name := contentDispositionName hds | return ← stop inner
  let contentType := headerContentType hds
  match contentDispositionFilename hds with
  | some filename =>
    return some (← fileEntry inner name filename contentType hds)
  | none =>
    if contentType.startsWith "text/" then
      let bodyBytes ← readUntil inner st.boundSepSearch
      return some (.field name ((String.fromUTF8? bodyBytes).getD ""))
    else
      let counter := st.nameCounter
      inner.modify fun s => { s with nameCounter := s.nameCounter + 1 }
      let filename := generatedFilename name counter contentType
      return some (← fileEntry inner name filename contentType hds)

end LeanIO
