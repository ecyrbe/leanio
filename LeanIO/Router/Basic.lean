import Std.Http
import Std.Async
import LeanIO.Router.RoutePattern
import LeanIO.Router.Route
import LeanIO.Router.RouteTrie

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
structure Router where
  routers     : Array (String × Router) := #[]
  routes      : Array Route := #[]
  middlewares : Array Middleware := #[]

/-- Creates an empty router with no routes, sub-routers or middlewares. -/
def Router.empty : Router := {}

/--
Applies an array of middlewares to a handler using left fold.
Used internally by `Router.toRouteTrie`.
-/
private def applyMiddlewares (ms : Array Middleware) : Middleware :=
  ms.foldl (fun h mw => mw h)

/--
Adds a single route to the router.

For an identical method and pattern, the **first** registration wins.
-/
def Router.addRoute (route : Route) (self : Router) : Router :=
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
def Router.addRouter (self : Router) (pre : String) (sub : Router) : Router :=
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
def Router.addMiddleware (mw : Middleware) (self : Router) : Router :=
  { self with middlewares := self.middlewares.push mw }

/--
Compiles the router tree into a flat `RouteTrie`.

For every route the handler is wrapped as
`router middlewares (… (sub-router middlewares (route middlewares handler)))`,
i.e. route-level middlewares are innermost, each enclosing router's middlewares
wrap around, and the outermost router's middlewares run first.

Own routes are inserted before sub-router routes; sub-router patterns get the
mount prefix prepended. All composition happens here, once — never at dispatch.
-/
partial def Router.toRouteTrie (self : Router) : RouteTrie :=
  let wrap := applyMiddlewares self.middlewares
  let trie := self.routes.foldr (fun route acc =>
    let h := applyMiddlewares route.middlewares route.handler
    acc.addRoute route.method route.pat.segments (wrap h)
  ) RouteTrie.empty
  self.routers.foldr (fun (pre, sub) acc =>
    let preSegs := (RoutePattern.ofString pre).segments
    RouteTrie.fold (Router.toRouteTrie sub) (fun method segs handler acc =>
      acc.addRoute method (preSegs ++ segs) (wrap handler)
    ) acc
  ) trie


instance : Coe Router RouteTrie where
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
def Router.serve (self : Router) (addr : Net.SocketAddress)
    (config : Config := {}) (backlog : UInt32 := 1024) : Async Server :=
  Server.serve addr self.toRouteTrie config backlog

end LeanIO.Router
