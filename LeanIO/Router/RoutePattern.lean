module

import Lean

namespace LeanIO.Router

/--
A single segment of a route pattern:
- `lit s`           — exact string match
- `param name`      — captures one path segment as `name`
- `rest name`       — catch-all (`{*name}`), captures the remainder of the path;
                       must be the last segment in a pattern
-/
public inductive Segment where
  | lit (s : String)
  | param (name : String)
  | rest (name : String)
deriving Repr, BEq, DecidableEq

public instance : ToString Segment where
  toString
    | Segment.lit s   => s!"lit \"{s}\""
    | Segment.param n => s!"param \"{n}\""
    | Segment.rest n  => s!"rest \"{n}\""

/-- A route pattern composed of `Segment` values, with precomputed length. -/
public structure RoutePattern where
  segments : List Segment
  length   : Nat
  hasRest  : Bool := false

public def splitPath (path : String) : List String :=
  path.split '/' |>.filter (¬ ·.isEmpty) |>.map toString |>.toList

public def RoutePattern.ofString (path : String) : RoutePattern :=
  let segs := splitPath path |>.map fun s =>
    if s.startsWith "{*" && s.endsWith "}" then
      Segment.rest (s.drop 2 |>.dropEnd 1 |>.toString)
    else if s.startsWith "{" && s.endsWith "}" then
      Segment.param (s.drop 1 |>.dropEnd 1 |>.toString)
    else
      Segment.lit s
  let hasRest := segs.any fun
    | Segment.rest _ => true | _ => false
  { segments := segs, length := segs.length, hasRest := hasRest }

end LeanIO.Router
