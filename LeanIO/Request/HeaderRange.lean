import Std.Async.ContextAsync
import LeanIO.Router.Route
import LeanIO.Request.FromRequestParts
import LeanIO.Data.HeaderName

namespace LeanIO
open Std.Http Std.Async

structure Range where
  start : Option Nat
  stop  : Option Nat
deriving Inhabited, Repr

/--
Parses the `Range` request header into an optional array of `Range` values.
Returns `none` if the header is missing or cannot be parsed.

Supports the standard HTTP range formats:
* `bytes=0-499`  — closed range
* `bytes=500-`   — open-ended (to end of file)
* `bytes=-500`   — suffix range (last N bytes)
* `bytes=0-499,1000-` — multiple ranges (only the first with lowest start is used)
-/
structure HeaderRange where
  ranges : Option (Array Range)
deriving Inhabited

private def parseOne (spec : String.Slice) : Option Range := do
    match (spec.split (· == '-')).toList with
    | [a, b] =>
      match a.trimAscii.toNat?,b.trimAscii.toNat? with
      | some a, some b => some ⟨some a, some b⟩
      | some a, none => some ⟨some a, none⟩
      | none, some b => some ⟨none, some b⟩
      | none,none => none
    | _ => none

private def parseRange (val : String) : Option (Array Range) :=
  val.trimAscii.dropPrefix? "bytes=" >>= (·.split (· == ',') |>.toArray |>.mapM parseOne)

instance : FromRequestParts HeaderRange where
  from_request_parts req :=
    match req.line.headers.get? .range with
    | some value => .ok { ranges := parseRange value.value }
    | none => .ok { ranges := none }

end LeanIO
