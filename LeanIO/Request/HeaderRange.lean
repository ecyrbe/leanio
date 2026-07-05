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

private def parseOne (spec : String) : Option Range := do
  let spec := spec.trimAscii
  if spec.startsWith "-" then
    let n ← spec.drop 1 |>.trimAscii |>.toNat?
    some { start := none, stop := some n }
  else if spec.endsWith "-" then
    let n ← spec.dropEnd 1 |>.trimAscii |>.toNat?
    some { start := some n, stop := none }
  else
    let parts := (spec.split (· == '-')).toList
    if parts.length == 2 then
      let s ← parts[0]? |>.bind (·.toString.trimAscii.toNat?)
      let e ← parts[1]? |>.bind (·.toString.trimAscii.toNat?)
      some { start := some s, stop := some e }
    else
      failure

private def parseRange (val : String) : Option (Array Range) := do
  let s := val.trimAscii
  unless s.startsWith "bytes=" do failure
  let specs := (s.drop 6 |>.split (· == ',')).toList |>.map (·.toString)
  let ranges ← specs.mapM parseOne
  pure (ranges.toArray)

instance : FromRequestParts HeaderRange where
  from_request_parts req :=
    match req.line.headers.get? .range with
    | some value => .ok { ranges := parseRange value.value }
    | none => .ok { ranges := none }

end LeanIO
