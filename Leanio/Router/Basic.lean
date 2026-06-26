import Std.Http
import Std.Async
import Leanio.Router.RoutePattern
import Leanio.Router.Route

namespace Leanio.Router
open Std Http Server
open Std.Async

def stripPathPrefix (full : String) (pre : String) : Option String := do
  if full == pre then
    return "/"
  else
    let pos ← full.skipPrefix? pre
    match ← pos.get? with
    | '/' => (full.sliceFrom pos).toString
    | _ => none

structure Router where
  routes      : List Route
  routers     : List (String × Router) := []
  middlewares : List (HandlerSig → HandlerSig) := []

def Router.empty : Router :=
  { routes := [], routers := [], middlewares := [] }

def Router.addRoute (route : Route) (r : Router) : Router :=
  { r with routes := r.routes ++ [route] }

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

def applyMiddlewares (ms : List (HandlerSig → HandlerSig)) (h : HandlerSig) : HandlerSig :=
  ms.foldl (fun h mw => mw h) h

partial def findRoute (router : Router) (methodRef : Method) (path : String) : Option HandlerSig := do
  for r in router.routes do
    if r.method = methodRef then
      match matchPath r.pat path with
      | some _ => return applyMiddlewares r.middlewares r.handler
      | none   => pure ()
  none

partial def dispatch (router : Router) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
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

instance : Handler Router where
  onRequest := dispatch

end Leanio.Router
