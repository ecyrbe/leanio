import Std.Http
import Std.Async
import Leanio.Router.RoutePattern
import Leanio.Router.Route

namespace Leanio.Router
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
  handlers : DHashMap.Raw Method (fun _ => HandlerSig) := ∅
  literals : DHashMap.Raw String (fun _ => RouteTrie)  := ∅
  param    : Option RouteTrie                           := none
  wildcard : Option RouteTrie                           := none

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
  | Segment.param _ :: rest =>
    let child := trie.param.getD empty
    { trie with param := some (addRoute child method rest handler) }
  | Segment.rest _ :: rest =>
    let child := trie.wildcard.getD empty
    { trie with wildcard := some (addRoute child method rest handler) }

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
Looks up a handler by method and path segments. Priority: literal > wildcard > catchall.

Returns `some (capturedParams, handler)` where `capturedParams` are in the order
they appear in the route pattern, or `none` if no route matches.
-/
def lookup (trie : RouteTrie) (method : Method) (segs : List String) : Option (List String × HandlerSig) :=
  go trie segs
where
  go (t : RouteTrie) (segs : List String) : Option (List String × HandlerSig) :=
    match segs with
    | [] => t.handlers.get? method |>.map (fun h => ([], h))
    | seg :: rest =>
      let litResult := t.literals.get? seg |>.bind fun child => go child rest
      let withParam := litResult.orElse fun _ =>
        t.param |>.bind fun child =>
          go child rest |>.map fun (vs, h) => (seg :: vs, h)
      withParam.orElse fun _ =>
        t.wildcard |>.bind fun child =>
          child.handlers.get? method |>.map fun h =>
            ([String.intercalate "/" (seg :: rest)], h)

end RouteTrie
end Leanio.Router
