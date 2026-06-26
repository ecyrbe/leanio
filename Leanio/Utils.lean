import Std.Http
import Leanio.Data.Redacted
import Leanio.Data.String

namespace Leanio.Utils
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

end Leanio.Utils
