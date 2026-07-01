import Std.Http
import LeanIO.Data.Redacted
import LeanIO.Data.String

namespace LeanIO.Utils
open Std.Http

def formatNanos (nanos : Nat) : String :=
  if nanos ≥ 1_000_000 then s!"{nanos / 1_000_000}ms"
  else if nanos ≥ 1_000 then s!"{nanos / 1_000}µs"
  else "< 1µs"

private def charToValue (c : Char) : Option UInt8 :=
  if 'A' ≤ c && c ≤ 'Z' then some <| c.toNat - 'A'.toNat |>.toUInt8
  else if 'a' ≤ c && c ≤ 'z' then some <| 26 + c.toNat - 'a'.toNat |>.toUInt8
  else if '0' ≤ c && c ≤ '9' then some <| 52 + c.toNat - '0'.toNat |>.toUInt8
  else if c = '+' || c = '-' then some 62
  else if c = '/' || c = '_' then some 63
  else if c = '=' then some 0
  else none

def base64Decode (encoded : String) : Option ByteArray := do
  let mut decoded := ByteArray.empty
  let mut chars := encoded.toList
  let rem := chars.length % 4
  if rem ≠ 0 then
    chars:= chars ++ List.replicate (4 - rem) '='
  while !chars.isEmpty do
    match chars with
    | c1 :: c2 :: c3 :: c4 :: rest =>
      let v1 ← charToValue c1
      let v2 ← charToValue c2
      let v3 ← charToValue c3
      let v4 ← charToValue c4
      decoded := decoded.push ((v1 <<< 2) ||| (v2 >>> 4))
      if c3 ≠ '=' then
        decoded := decoded.push (((v2 &&& 0x0F) <<< 4) ||| (v3 >>> 2))
      if c4 ≠ '=' then
        decoded := decoded.push (((v3 &&& 0x03) <<< 6) ||| v4)
      chars := rest
    | _ => none -- should not happen
  return decoded

def base64DecodeString (encoded : String) : Option String := do
  String.fromUTF8? <| ← base64Decode encoded


def parseBasicAuth (auth: String): Option (String × Redacted) := do
  let basic ← auth.dropPrefix? "Basic " |>.map (·.trimAscii |>.copy)
  let decoded ← base64DecodeString basic
  decoded.splitOnce ':' |>.map fun (a,b) => (a,↑b)

def parseBearer (auth: String): Option String :=
  auth.dropPrefix? "Bearer " |>.map (·.trimAscii |>.toString)

def extractAuthorization (request: Request α): Option String :=
  request.line.headers.get? Header.Name.authorization |>.map (·.value)

end LeanIO.Utils

namespace LeanIO

private def buildLPS (needle : ByteArray) : Array Nat := Id.run do
  let mut lps := Array.ofFn (fun (_ : Fin needle.size) => 0)
  let mut len := 0
  let mut i := 1
  while i < needle.size do
    if needle.get! i = needle.get! len then
      len := len + 1
      lps := lps.set! i len
      i := i + 1
    else if len = 0 then
      i := i + 1
    else
      len := lps[len - 1]!
      pure ()
  return lps

partial def indexOfSubarrayKMP (haystack : ByteArray) (needle : ByteArray) (start : Nat := 0) : Option Nat :=
  if needle.size = 0 then some start
  else if start + needle.size > haystack.size then none
  else
    let lps := buildLPS needle
    let rec go (i j : Nat) : Option Nat :=
      if i ≥ haystack.size then none
      else if haystack.get! i = needle.get! j then
        if j + 1 = needle.size then some (i - j)
        else go (i + 1) (j + 1)
      else if j = 0 then go (i + 1) 0
      else go i (lps[j - 1]!)
    go start 0

end LeanIO
