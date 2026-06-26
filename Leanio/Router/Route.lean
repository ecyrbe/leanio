import Std.Http
import Std.Async
import Leanio.Router.RoutePattern

namespace Leanio.Router
open Std Http Server
open Std.Async

abbrev HandlerSig := Request Body.Stream → ContextAsync (Response Body.Any)

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

end Leanio.Router
