import Std.Http
import Std.Async
import LeanIO.Router.RoutePattern
import LeanIO.Router.Route

namespace LeanIO.Router
open Std Http Server
open Std.Async

deriving instance Hashable for Method
deriving instance BEq for Method
deriving instance ReflBEq for Method
deriving instance LawfulBEq for Method

/--
A segment-based trie for O(depth) route dispatch instead of O(routes) linear scan.

Each node holds handlers at this path depth, literal edges (exact segment match),
a param edge (`{param}`) for single-segment path parameters, and a wildcard edge
(`{*rest}`) for lowest-priority remainder capture.

Priority: literal > param (`{param}`) > wildcard (`{*rest}`)

Uses `DHashMap.Raw` for handlers and literals — the raw hash map type
avoids Lean's kernel positivity check with recursive value types.
-/
structure RouteTrie where
  handlers : HashMap Method HandlerSig := ∅
  literals : HashMap.Raw String RouteTrie  := ∅
  param    : Option (String × RouteTrie)                := none
  wildcard : Option (String × RouteTrie)                := none

namespace RouteTrie

/-- Empty trie with no routes. -/
def empty : RouteTrie := {}

/--
Inserts a route into the trie given its method, segment list, and composed handler.

Middlewares should be composed onto the handler before insertion.
-/
def addRoute (trie : RouteTrie) (method : Method) (segs : List Segment) (handler : HandlerSig) : RouteTrie :=
  match segs with
  | [] => { trie with handlers := trie.handlers.insert method handler }
  | Segment.lit s :: rest =>
    let child := trie.literals.get? s |>.getD empty
    { trie with literals := trie.literals.insert s (addRoute child method rest handler) }
  | Segment.param name :: rest =>
    let child := match trie.param with | some (_, c) => c | none => empty
    { trie with param := some (name, addRoute child method rest handler) }
  | Segment.rest name :: rest =>
    let child := match trie.wildcard with | some (_, c) => c | none => empty
    { trie with wildcard := some (name, addRoute child method rest handler) }

/--
Adds a route from a runtime pattern string (e.g. `"/user/{id}"`).
Useful for programmatic (non-macro) route construction.
-/
def addRouteFromPattern (trie : RouteTrie) (method : Method) (pattern : String) (handler : HandlerSig) : RouteTrie :=
  let pat := RoutePattern.ofString pattern
  addRoute trie method pat.segments handler

/--
Builds a trie from a list of `Route` values, composing route-level middlewares.
-/
def ofRoutes (routes : List Route) : RouteTrie :=
  routes.foldl (init := empty) fun trie r =>
    let h := r.middlewares.foldl (fun f mw => mw f) r.handler
    trie.addRoute r.method r.pat.segments h

/--
Looks up a handler by method and path segments. Priority: literal > param > wildcard.

Returns `some (capturedParams, handler)` where `capturedParams` maps param names
to values, or `none` if no route matches.
-/
def lookup (trie : RouteTrie) (method : Method) (segs : List String) : Option ((HashMap String String) × HandlerSig) :=
  go trie segs ∅
where
  go (t : RouteTrie) (segs : List String) (acc : HashMap String String) : Option ((HashMap String String) × HandlerSig) :=
    match segs with
    | [] => t.handlers.get? method |>.map (fun h => (acc, h))
    | seg :: rest =>
      let litResult := t.literals.get? seg |>.bind fun child => go child rest acc
      let withParam := litResult.orElse fun _ =>
        t.param |>.bind fun (name, child) =>
          go child rest (acc.insert name seg)
      withParam.orElse fun _ =>
        t.wildcard |>.bind fun (name, child) =>
          child.handlers.get? method |>.map fun h =>
            (acc.insert name (String.intercalate "/" (seg :: rest)), h)

/--
Walks the trie depth-first, calling `f method segs handler acc` for every stored
handler, where `segs` is the reconstructed path of `Segment` values leading to it
with the original parameter names preserved.

Example: given a trie containing

  ```
  GET /todos             → h1
  POST /todos            → h2
  GET /todos/{id}        → h3
  ```

`fold trie (fun m segs h acc => (m, segs) :: acc) []` produces

  ```lean4
  [(GET,  [lit "todos", param "id"]),
   (POST, [lit "todos"]),
   (GET,  [lit "todos"])]
  ```

(order is deterministic but insertion-dependent, not route-priority).
-/
partial def fold (trie : RouteTrie) (f : Method → List Segment → HandlerSig → α → α) (init : α) : α :=
  foldGo f trie [] init
where
  foldGo (f : Method → List Segment → HandlerSig → α → α) (t : RouteTrie) (revSegs : List Segment) (acc : α) : α :=
    let acc := HashMap.fold (fun acc m h => f m revSegs.reverse h acc) acc t.handlers
    let acc := HashMap.Raw.fold (fun acc s child => foldGo f child (Segment.lit s :: revSegs) acc) acc t.literals
    let acc := match t.param with | none => acc | some (name, child) => foldGo f child (Segment.param name :: revSegs) acc
    let acc := match t.wildcard with | none => acc | some (name, child) => foldGo f child (Segment.rest name :: revSegs) acc
    acc

end LeanIO.Router.RouteTrie
