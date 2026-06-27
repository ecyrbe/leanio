import Std.Http
import Std.Async
import Leanio.Router.RoutePattern
import Leanio.Router.Route
import Leanio.Router.RouteTrie

namespace Leanio.Router
open Std Http Server
open Std.Async

/--
A router holds a segment trie for O(depth) route dispatch
and middlewares applied across the whole router.

Sub-routers are merged into the trie at construction time — no fallback scan at dispatch.
-/
structure Router where
  trie        : RouteTrie := RouteTrie.empty
  middlewares : List (HandlerSig → HandlerSig) := []

/-- Creates an empty router with no routes or middlewares. -/
def Router.empty : Router := {}

/--
Applies a list of middlewares to a handler using left fold.
Used internally by `addRoute`, `addRouter` and `dispatch`.
-/
private def applyMiddlewares (ms : List (HandlerSig → HandlerSig)) (h : HandlerSig) : HandlerSig :=
  ms.foldl (fun h mw => mw h) h

/--
Adds a single route to the router, inserting it into the segment trie.
Route-level middlewares are pre-composed onto the handler at insertion time.

Earlier inserts have higher priority for conflicting patterns (first match wins).
-/
def Router.addRoute (route : Route) (r : Router) : Router :=
  let h := applyMiddlewares route.middlewares route.handler
  { r with trie := r.trie.addRoute route.method route.pat.segments h }

/--
Merges `sub` into `r` under the prefix `pre`.

All routes from `sub` (including recursively merged sub-routers) are inserted
into `r`'s trie with `pre` prepended to their patterns. `sub`'s middlewares
are composed onto each merged handler.

```lean4
Router.empty |>.addRouter "/api/v1" todosRouter
```
-/
def Router.addRouter (r : Router) (pre : String) (sub : Router) : Router :=
  let prePat := RoutePattern.ofString pre
  let preSegs := prePat.segments
  let wrapHandler := applyMiddlewares sub.middlewares
  let mergedTrie :=
    RouteTrie.fold sub.trie (fun method segs handler acc =>
      acc.addRoute method (preSegs ++ segs) (wrapHandler handler)
    ) r.trie
  { r with trie := mergedTrie }

/--
Appends a middleware to the router's middleware list.

Middlewares are applied with `foldl`, so the **last** middleware added runs **first** (outermost).
In other words, `router.addMiddleware A |>.addMiddleware B` results in `B` wrapping `A`.

Example:
```lean4
  -- catchErrors runs first (outermost), then requestLogger, then the handler
  router.addMiddleware requestLogger
    |>.addMiddleware catchErrors
```
-/
def Router.addMiddleware (mw : HandlerSig → HandlerSig) (r : Router) : Router :=
  { r with middlewares := r.middlewares ++ [mw] }

/--
Looks up a handler in the trie by method and path segments. O(depth) instead of O(routes).
-/
private def findRoute (router : Router) (methodRef : Method) (pathSegs : List String) : Option ((HashMap String String) × HandlerSig) :=
  router.trie.lookup methodRef pathSegs

/--
Dispatches an incoming request through the trie. All routes and sub-routes
are merged into the trie at construction time, so dispatch is a single lookup.

If no route matches, returns 404.
-/
private partial def dispatch (router : Router) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  let path := toString req.line.uri.path
  let pathSegs := (splitPath path)
  match findRoute router req.line.method pathSegs with
  | some (params, handler) =>
    let req' := { req with extensions := req.extensions.insert { params : RouteParams } }
    let wrapped := applyMiddlewares router.middlewares handler
    wrapped req'
  | none =>
    Response.notFound |>.text s!"404 Not Found: {req.line.method} {path}"

/-- Makes `Router` usable as a `Std.Http.Server.Handler`. -/
instance : Handler Router where
  onRequest := dispatch

end Leanio.Router
