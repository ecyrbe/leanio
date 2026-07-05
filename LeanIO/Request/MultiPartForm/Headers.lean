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

/--
Parse a parameter value, handling quoted strings with escaped quotes.

- `abc` → `abc`
- `"abc"` → `abc`
- `"ab\"c"` → `ab"c"
-/
def parseParamValue (s : String.Slice) : Option String :=
  let inner := s.toString
  if inner.startsWith "\"" then
    let chars := inner.toRawSubstring.drop 1
    let rec go (i : Nat) (acc : String) : Option String :=
      if i ≥ chars.bsize then none
      else
        let c := chars.get ⟨i⟩
        if c == '\\' then
          if i + 1 < chars.bsize then
            go (i + 2) (acc ++ toString (chars.get ⟨i + 1⟩))
          else none
        else if c == '\"' then
          some acc
        else
          go (i + 1) (acc ++ toString c)
    go 0 ""
  else
    let unquoted := inner.takeWhile (fun c => c ≠ ';' && c ≠ ' ' && c ≠ ',')
    some unquoted.toString

/-- Extract a parameter from a semicolon-separated header value (Content-Type, Content-Disposition, etc). -/
def extractParam (params : String) (key : String) : Option String :=
  let keySuffix := key ++ "="
  params.split (· == ';')
  |>.findSome? (fun part =>
    part.trimAscii.dropPrefix? keySuffix
    |>.bind parseParamValue)
  |>.bind fun s => if s.isEmpty then none else some s

/-- Extract a parameter value from a form-data content disposition header. -/
def filenameParam (hds : Headers) : Option String :=
  match hds.get? contentDisposition with
  | none => none
  | some v => extractParam v.value "filename"

/-- Extract a parameter value from a form-data content disposition header. -/
def nameParam (hds : Headers) : Option String :=
  match hds.get? contentDisposition with
  | none => none
  | some v => extractParam v.value "name"

/-- Extract the Content-Type from headers, defaulting to `text/plain` per RFC 2046 §5.1. -/
def headerContentType (hds : Headers) : String :=
  match hds.get? .contentType with
  | some v => v.value
  | none => "text/plain"

end LeanIO
