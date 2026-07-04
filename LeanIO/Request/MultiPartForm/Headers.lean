import Lean
import LeanIO.Utils
import LeanIO.Data.ChunkBuffer
import LeanIO.Data.String

namespace LeanIO
open Std.Http Std.Slice

abbrev contentDisposition := Header.Name.mk "content-disposition"

/-- Parse a header line "Name: value" into a `(Name, Value)` pair. Splits on first colon only. -/
def parseOneHeader (line : String) : Option (Header.Name × Header.Value) :=
  match String.splitOnce line ':' with
  | none => none
  | some (name, value) =>
    let name := name.trimAscii.toString
    let val := value.trimAscii.toString
    match Header.Name.ofString? name, Header.Value.ofString? val with
    | some name, some value => some (name, value)
    | _,_ => none

/-- Parse raw header bytes into `Std.Http.Headers`. Returns `none` on parse failure. -/
def parseHeaders (hdrBytes : ByteArray) : Option Headers := do
  let hdrStr ← String.fromUTF8? hdrBytes
  (hdrStr.splitOn "\r\n").foldlM (fun (hds : Headers) (line : String) =>
    let clean := line.trimAscii.toString
    if clean.isEmpty then some hds else
      match parseOneHeader clean with
      | some (n, v) => some (hds.insert n v)
      | none => none
    ) (∅ : Headers)

/-- Extract a quoted parameter value from a Content-Disposition value string. -/
def extractParam (params : String) (key : String) : Option String :=
  let keySuffix := key ++ "=\""
  (params.split (· == ';')).findSome? fun part =>
    let trimmed := part.trimAscii
    trimmed.dropPrefix? keySuffix
    |>.map (·.takeWhile (· ≠ '\"'))
    |>.bind fun s =>
        if s.isEmpty then
          none
        else
          some s.toString

/-- Extract the `name` parameter from Content-Disposition headers. -/
def contentDispositionName (hds : Headers) : Option String :=
  match hds.get? contentDisposition with
  | none => none
  | some v => extractParam v.value "name"

/-- Extract the `filename` parameter from Content-Disposition headers. -/
def contentDispositionFilename (hds : Headers) : Option String :=
  match hds.get? contentDisposition with
  | none => none
  | some v => extractParam v.value "filename"

/-- Extract the Content-Type from headers, defaulting to `text/plain` per RFC 2046 §5.1. -/
def headerContentType (hds : Headers) : String :=
  match hds.get? .contentType with
  | some v => v.value
  | none => "text/plain"

end LeanIO
