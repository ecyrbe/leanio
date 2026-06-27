import Std.Http
import Std.Async
import Leanio.Router.RoutePattern

namespace Leanio.Router
open Std Http Server
open Std.Async

abbrev HandlerSig := Request Body.Stream → ContextAsync (Response Body.Any)

/-- Carries captured path parameters through request extensions. -/
structure RouteParams where
  values : Array String
deriving TypeName, Inhabited

structure Route where
  method     : Method
  pat        : RoutePattern
  handler    : HandlerSig
  middlewares : List (HandlerSig → HandlerSig) := []

/--
Appends a middleware to this route. Route middlewares run **after** router-level middlewares,
just before the handler.

Example:
```lean4
  GET "/admin" adminRoute (req : Request Body.Stream) := ...
  def router := Router.empty |>.addRoute (adminRoute.addMiddleware auth)
```
-/
def Route.addMiddleware (mw : HandlerSig → HandlerSig) (route : Route) : Route :=
  { route with middlewares := route.middlewares ++ [mw] }

/--
Runtime route constructor. Prefer the `GET`/`POST`/... term macros for
compile-time pattern validation and param extraction. Use this only when
the handler is already fully built.

```lean4
def myHandler : HandlerSig := fun req => do
  let params := (req.extensions.get Leanio.Router.RouteParams).getD { values := #[] }
  let id := match params.values.get? 0 with | some n => n | none => "unknown"
  Response.ok |>.text s!"user {id}"

def myRoute : Route := Route.new .get "/user/{id}" myHandler
```
-/
def Route.new (method : Method) (pattern : String) (handler : HandlerSig) : Route :=
  { method, handler, pat := RoutePattern.ofString pattern }

end Leanio.Router
