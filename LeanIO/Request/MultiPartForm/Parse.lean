import Std.Async.ContextAsync
import LeanIO.Data.MimeType
import LeanIO.Request.MultiPartForm.Defs
import LeanIO.Request.MultiPartForm.Headers
import LeanIO.Request.MultiPartForm.Stream

namespace LeanIO
open Std.Http Std.Async Std.Slice MimeType

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

def extractBoundary (contentType : String) : Option String :=
  (contentType.split (· = ';')).toList.filterMap (fun s =>
    (s.toString.trimAscii.dropPrefix? "boundary=").map fun val =>
      val.takeWhile (fun c => c ≠ ';' && c ≠ ' ' && c ≠ '\"') |>.toString)
  |>.head?

/-- Build a generated filename: `{name}{counter}.{ext}`. -/
def generatedFilename (name : String) (counter : Nat) (mime : String) : String :=
  s!"{name}{counter}.{extForMime mime}"


def parseNextEntry (inner : IO.Ref MultipartInner) : ContextAsync (Option MultipartEntry) := do
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

end LeanIO
