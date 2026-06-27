import Lean

namespace Leanio.Router

/--
A single segment of a route pattern: either a literal string to match exactly,
or a path parameter `{name}` that captures the corresponding URL segment.
-/
inductive Segment where
  | lit (s : String)
  | param (name : String)
deriving Repr, BEq, DecidableEq

instance : ToString Segment where
  toString
    | Segment.lit s => s!"lit \"{s}\""
    | Segment.param name => s!"param \"{name}\""

/-- A route pattern composed of `Segment` values, e.g. `["user", param "id"]`. -/
structure RoutePattern where
  segments : List Segment
  length   : Nat

def splitPath (path : String) : List String :=
  path.split '/' |>.filter (¬ ·.isEmpty) |>.map toString |>.toList

def RoutePattern.ofString (path : String) : RoutePattern :=
  let segs := splitPath path |>.map fun s =>
    if s.startsWith '{' && s.endsWith '}' then
      Segment.param (s.drop 1 |>.dropEnd 1 |>.toString)
    else
      Segment.lit s
  { segments := segs, length := segs.length }

private def matchImpl
  (pattern : RoutePattern) (seg : List String) : Option (List String) := do
  if pattern.length ≠ seg.length then failure
  let mut acc : List String := []
  for (p, s) in pattern.segments.zip seg do
    match p with
    | Segment.lit lit => if lit ≠ s then failure
    | Segment.param _ => acc := s :: acc
  return acc.reverse

/--
Matches a URL path against a route pattern's segments, returning the captured
parameter values in order.

Literal segments must match exactly; `{param}` segments capture the value.

```lean4
matchPath (RoutePattern.ofString "/user/{id}") "/user/42"   -- some ["42"]
matchPath (RoutePattern.ofString "/user/{id}") "/hello"      -- none
```
-/
def RoutePattern.matchPath (pattern : RoutePattern) (path : String) : Option (List String) :=
  matchImpl pattern (splitPath path)

def RoutePattern.matchPathSegments (pattern : RoutePattern) (segs : List String) : Option (List String) :=
  matchImpl pattern segs

end Leanio.Router
