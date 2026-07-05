module

public import LeanIO.Data.Redacted
import LeanIO.Data.String

namespace LeanIO.Utils
open LeanIO

public def formatNanos (nanos : Nat) : String :=
  if nanos ≥ 1_000_000 then s!"{nanos / 1_000_000}ms"
  else if nanos ≥ 1_000 then s!"{nanos / 1_000}µs"
  else "< 1µs"

def charToValue (c : Char) : Option UInt8 :=
  if 'A' ≤ c && c ≤ 'Z' then some <| c.toNat - 'A'.toNat |>.toUInt8
  else if 'a' ≤ c && c ≤ 'z' then some <| 26 + c.toNat - 'a'.toNat |>.toUInt8
  else if '0' ≤ c && c ≤ '9' then some <| 52 + c.toNat - '0'.toNat |>.toUInt8
  else if c = '+' || c = '-' then some 62
  else if c = '/' || c = '_' then some 63
  else if c = '=' then some 0
  else none

public def base64Decode (encoded : String) : Option ByteArray := do
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

public def base64DecodeString (encoded : String) : Option String := do
  String.fromUTF8? <| ← base64Decode encoded


public def parseBasicAuth (auth: String): Option (String × Redacted) := do
  let basic ← auth.dropPrefix? "Basic " |>.map (·.trimAscii |>.copy)
  let decoded ← base64DecodeString basic
  decoded.splitOnce ':' |>.map fun (a,b) => (a.toString,↑b.toString)

public def parseBearer (auth: String): Option String :=
  auth.dropPrefix? "Bearer " |>.map (·.trimAscii |>.toString)


end LeanIO.Utils

namespace LeanIO

public structure Search (α: Type) where
  needle: α
  LPS : Array Nat

namespace Search

  public class Sized (α: Type) where
    size : α → Nat

  public instance : Sized ByteArray where
   size:= ByteArray.size

  @[specialize]
  public def new {α : Type } {dom: α → Nat → Prop} [BEq el] [DecidableEq el] [Inhabited el] [GetElem? α Nat el dom] [Sized α] (needle: α) : Search α:= Id.run do
    let mut LPS := Array.replicate (Sized.size needle) 0
    let mut len := 0
    let mut i := 1
    while i < (Sized.size needle) do
      if needle[i]! = needle[len]! then
        len := len + 1
        LPS := LPS.set! i len
        i := i + 1
      else if len = 0 then
        i := i + 1
      else
        len := LPS[len - 1]!
    return {needle, LPS}

  @[specialize]
  public partial def search {α : Type } {dom: α → Nat → Prop} [BEq el] [DecidableEq el] [Inhabited el] [GetElem? α Nat el dom] [Sized α] (self: Search α) (haystack: α) (start: Nat := 0): Option Nat := do
    if Sized.size self.needle = 0 then
      return start
    else if start + Sized.size self.needle > (Sized.size haystack) then
      none
    else
      let rec go (i j : Nat) : Option Nat :=
        if i ≥ (Sized.size haystack) then none
        else if haystack[i]! = self.needle[j]! then
          if j + 1 = (Sized.size self.needle) then some (i - j)
          else go (i + 1) (j + 1)
        else if j = 0 then go (i + 1) 0
        else go i (self.LPS[j - 1]!)
      go start 0

  /-- Compute the longest prefix-suffix overlap between the end of `haystack` and `needle`.
      Returns the number of trailing bytes that form a prefix of `needle`.
      Runs KMP on only the last `needle.size - 1` bytes. -/
  public partial def terminalOverlap {α : Type } {dom: α → Nat → Prop} [BEq el] [DecidableEq el] [Inhabited el] [GetElem? α Nat el dom] [Sized α] (self: Search α) (haystack: α) (start: Nat) : Nat :=
    let n := Sized.size self.needle
    if n = 0 then 0
    else
      let available := Sized.size haystack - start
      let scanStart := if available ≥ n then available - n + 1 else 0
      let rec go (i j : Nat) (endPoint : Nat) : Nat :=
        if i ≥ endPoint then j
        else if haystack[i]! = self.needle[j]! then
          go (i + 1) (j + 1) endPoint
        else if j = 0 then go (i + 1) 0 endPoint
        else go i (self.LPS[j - 1]!) endPoint
      go (start + scanStart) 0 (Sized.size haystack)

end Search

end LeanIO
