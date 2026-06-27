import Lean

namespace Leanio.Router

/--
A single segment of a route pattern:
- `lit s`           — exact string match
- `param name`      — captures one path segment as `name`
- `rest name`       — catch-all (`{*name}`), captures the remainder of the path;
                       must be the last segment in a pattern
-/
inductive Segment where
  | lit (s : String)
  | param (name : String)
  | rest (name : String)
deriving Repr, BEq, DecidableEq

instance : ToString Segment where
  toString
    | Segment.lit s   => s!"lit \"{s}\""
    | Segment.param n => s!"param \"{n}\""
    | Segment.rest n  => s!"rest \"{n}\""

/-- A route pattern composed of `Segment` values, with precomputed length. -/
structure RoutePattern where
  segments : List Segment
  length   : Nat
  hasRest  : Bool := false

def splitPath (path : String) : List String :=
  path.split '/' |>.filter (¬ ·.isEmpty) |>.map toString |>.toList

def RoutePattern.ofString (path : String) : RoutePattern :=
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

private def matchImpl
  (pattern : RoutePattern) (segs : List String) : Option (List String) :=
  go pattern.segments segs []
where
  go (pat : List Segment) (segs : List String) (acc : List String) : Option (List String) :=
    match pat, segs with
    | [], [] => some acc.reverse
    | Segment.rest _ :: _, segs => some ((String.intercalate "/" segs) :: acc |>.reverse)
    | Segment.lit l :: ps, s :: ss => if l == s then go ps ss acc else none
    | Segment.param _ :: ps, s :: ss => go ps ss (s :: acc)
    | _, _ => none

/--
Matches a URL path against a route pattern's segments, returning the captured
parameter values in order.

Literal segments must match exactly; `{param}` segments capture one value;
`{*rest}` captures the remainder as a `/`-joined string.
```
matchPath (RoutePattern.ofString "/user/{id}") "/user/42"       -- some ["42"]
matchPath (RoutePattern.ofString "/files/{*path}") "/files/a/b" -- some ["a/b"]
```
-/
def RoutePattern.matchPath (pattern : RoutePattern) (path : String) : Option (List String) :=
  matchImpl pattern (splitPath path)

def RoutePattern.matchPathSegments (pattern : RoutePattern) (segs : List String) : Option (List String) :=
  matchImpl pattern segs

end Leanio.Router
