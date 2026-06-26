import Std.Http
import Std.Async
import Leanio.Router.RoutePattern
import Leanio.Router.Route

namespace Leanio.Router
open Std Http Server
open Std.Async

/--
Strips `pre` from the start of `path` at a path-segment boundary.
Returns the remainder with a leading `/`, or `none` if `pre` is not a valid prefix.

```lean4
stripPathPrefix "/api/user" "/api"        -- some "/user"
stripPathPrefix "/api" "/api"             -- some "/"
stripPathPrefix "/apix" "/api"            -- none
```
-/
def stripPathPrefix (path : String) (pre : String) : Option String := do
  if path == pre then
    return "/"
  else
    let pos ← path.skipPrefix? pre
    match ← pos.get? with
    | '/' => (path.sliceFrom pos).toString
    | _ => none

/--
A router holds registered routes, sub-routers (mounted under prefixes),
and middlewares applied across the whole router.
-/
structure Router where
  routes      : List Route
  routers     : List (String × Router) := []
  middlewares : List (HandlerSig → HandlerSig) := []

/-- Creates an empty router with no routes, sub-routers, or middlewares. -/
def Router.empty : Router :=
  { routes := [], routers := [], middlewares := [] }

/--
Adds a single route to the router. Routes are matched in insertion order
(first match wins).
-/
def Router.addRoute (route : Route) (r : Router) : Router :=
  { r with routes := r.routes ++ [route] }

/--
Mounts `sub` under the prefix `pre`. Requests whose path starts with `pre`
are forwarded to `sub` with the prefix stripped.

```lean4
Router.empty |>.addRouter "/api/v1" todosRouter
```
-/
def Router.addRouter (r : Router) (pre : String) (sub : Router) : Router :=
  { r with routers := r.routers ++ [(pre, sub)] }

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
Applies a list of middlewares to a handler using left fold.
Used internally by `findRoute` and `dispatch`.
-/
private def applyMiddlewares (ms : List (HandlerSig → HandlerSig)) (h : HandlerSig) : HandlerSig :=
  ms.foldl (fun h mw => mw h) h

/--
Searches registered routes for the first one matching `methodRef` and `path`.
Returns the handler with route-level middlewares applied, or `none` if no route matches.
-/
private def findRoute (router : Router) (methodRef : Method) (path : String) : Option HandlerSig := do
  for r in router.routes do
    if r.method = methodRef then
      match matchPath r.pat path with
      | some _ => return applyMiddlewares r.middlewares r.handler
      | none   => pure ()
  none

/--
Dispatches an incoming request through the router.

First checks registered routes and sub-routers.  If a matching route is found,
its handler (wrapped in route and router middlewares) is invoked.  Otherwise
returns a 404 response.
-/
private partial def dispatch (router : Router) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  let path := toString req.line.uri.path
  match findRoute router req.line.method path with
  | some h =>
    let wrapped := applyMiddlewares router.middlewares h
    wrapped req
  | none   =>
    let mut result : Option (ContextAsync (Response Body.Any)) := none
    for (pre, sub) in router.routers do
      match stripPathPrefix path pre with
      | some remaining =>
        let handler : HandlerSig := fun req' =>
          let modifiedReq' : Request Body.Stream :=
            { req' with line := { req'.line with uri := RequestTarget.parse! remaining } }
          dispatch sub modifiedReq'
        let wrapped := applyMiddlewares router.middlewares handler
        result := some (wrapped req); break
      | none => pure ()
    match result with
    | some r => r
    | none => Response.notFound |>.text s!"404 Not Found: {req.line.method} {path}"

/-- Makes `Router` usable as a `Std.Http.Server.Handler`. -/
instance : Handler Router where
  onRequest := dispatch

end Leanio.Router
