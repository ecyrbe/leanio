module

public import Std.Http
public import Std.Async
import LeanIO.Router.RoutePattern
public import LeanIO.Router.Route
public import LeanIO.Router.RouteTrie

namespace LeanIO.Router
open Std Http Server
open Std.Async

/--
A declarative router: an array of mounted sub-routers, an array of routes and
an array of middlewares. Nothing is composed at registration time.

`Router.serve` (or `Router.toRouteTrie`) compiles the whole tree into a
`RouteTrie` once, pre-composing all middlewares onto the handlers, so request
dispatch is a single O(depth) trie lookup with zero per-request composition.
-/
public structure Router where
  routers     : Array (String × Router) := #[]
  routes      : Array Route := #[]
  middlewares : Array Middleware := #[]
  /-- Serves any request matching no route, whatever its method. -/
  notFound    : Option HandlerFn := none

/-- Creates an empty router with no routes, sub-routers or middlewares. -/
public def Router.empty : Router := {}

/--
Applies an array of middlewares to a handler using left fold.
Used internally by `Router.toRouteTrie`.
-/
public def applyMiddlewares (ms : Array Middleware) : Middleware :=
  ms.foldl (fun h mw => mw h)

/--
Adds a single route to the router.

For an identical method and pattern, the **first** registration wins.
-/
public def Router.addRoute (route : Route) (self : Router) : Router :=
  { self with routes := self.routes.push route }

/--
Mounts `sub` under the prefix `pre`.

The sub-router is kept as-is and only merged into the trie by `toRouteTrie`,
where all its routes (including recursively mounted sub-routers) get `pre`
prepended to their patterns and `sub`'s middlewares composed onto their handlers.

```lean4
Router.empty |>.addRouter "/api/v1" todosRouter
```
-/
public def Router.addRouter (self : Router) (pre : String) (sub : Router) : Router :=
  { self with routers := self.routers.push (pre, sub) }

/--
Appends a middleware to the router's middleware array.

Middlewares are applied with `foldl`, so the **last** middleware added runs **first** (outermost).
In other words, `router.addMiddleware A |>.addMiddleware B` results in `B` wrapping `A`.

Example:
```lean4
  -- catchErrors runs first (outermost), then requestLogger, then the handler
  router.addMiddleware requestLogger
    |>.addMiddleware catchErrors
```
-/
public def Router.addMiddleware (mw : Middleware) (self : Router) : Router :=
  { self with middlewares := self.middlewares.push mw }

/--
Sets the handler for requests matching no route.

Unlike a catch-all route, this is not method-specific, so a JSON API keeps one
error shape for every miss rather than only for the methods it happened to
register.
-/
public def Router.withNotFound (handler : HandlerFn) (self : Router) : Router :=
  { self with notFound := some handler }

/--
Every route in the tree, in declaration order: a router's own routes come before
those of the routers mounted under it, and each group keeps its registration order.
Mount prefixes are prepended and all middlewares are composed here.

Handlers are wrapped as
`router middlewares (… (sub-router middlewares (route middlewares handler)))`,
i.e. route-level middlewares are innermost, each enclosing router's middlewares
wrap around, and the outermost router's middlewares run first.
-/
public def Router.flatten (self : Router) : Array (Method × List Segment × HandlerFn) :=
  let wrap := applyMiddlewares self.middlewares
  let own := self.routes.map fun route =>
    (route.method, route.pat.segments, wrap (applyMiddlewares route.middlewares route.handler))
  let subs := self.routers.attach.flatMap fun ⟨(pre, sub), _h⟩ =>
    let preSegs := (RoutePattern.ofString pre).segments
    sub.flatten.map fun (method, segs, handler) => (method, preSegs ++ segs, wrap handler)
  own ++ subs
decreasing_by
  have := Array.sizeOf_lt_of_mem _h
  cases self
  simp_all
  omega

/--
Compiles the router tree into a flat `RouteTrie`.

Routes are inserted in reverse declaration order, so where two of them claim the
same method and pattern the first one declared is the one that serves it. That
holds uniformly, whether the loser was registered on this router or on one mounted
under it. All composition happens here, once — never at dispatch.

Only the root's `notFound` is consulted, so a mounted router's own fallback is
discarded rather than serving misses below its prefix.
-/
public def Router.toRouteTrie (self : Router) : RouteTrie :=
  let trie := self.flatten.foldr (fun (method, segs, handler) acc =>
    acc.addRoute method segs handler) RouteTrie.empty
  { trie with notFound := self.notFound.map (applyMiddlewares self.middlewares) }


public instance : Coe Router RouteTrie where
  coe := Router.toRouteTrie

/--
Compiles the router into a `RouteTrie` and starts an HTTP server on `addr`.

Dispatch is handled by the `Handler RouteTrie` instance: a single trie lookup
per request, with all middlewares already composed onto the handlers.

```lean4
let server ← router.serve addr
server.waitShutdown
```
-/
public def Router.serve (self : Router) (addr : Net.SocketAddress)
    (config : Config := {}) (backlog : UInt32 := 1024) : Async Server :=
  Server.serve addr self.toRouteTrie config backlog

end LeanIO.Router
