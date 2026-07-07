module

public import Std.Http
public import LeanIO.Router.RoutePattern

namespace LeanIO.Router
open Std Http Server
open Std.Async

public abbrev HandlerSig := Request Body.Stream → ContextAsync (Response Body.Any)

/-- Carries captured path parameters through request extensions. -/
public structure RouteParams where
  params : List (String × String)
deriving TypeName, Inhabited

public def RouteParams.lookup (p : RouteParams) (key : String) : Option String :=
  p.params.find? (·.1 == key) |>.map (·.2)

public structure Route where
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
public def Route.addMiddleware (mw : HandlerSig → HandlerSig) (route : Route) : Route :=
  { route with middlewares := route.middlewares ++ [mw] }

/--
Runtime route constructor. Prefer the `GET`/`POST`/... term macros for
compile-time pattern validation and extractor support.
Use this only when the handler is already built as a `HandlerSig`.
-/
public def Route.new (method : Method) (pattern : String) (handler : HandlerSig) : Route :=
  { method, handler, pat := RoutePattern.ofString pattern }

end LeanIO.Router
