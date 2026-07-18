module

public import Std.Http
public import LeanIO.Router.RoutePattern

namespace LeanIO.Router
open Std Http Server
open Std.Async

public abbrev HandlerFn := Request Body.Stream → ContextAsync (Response Body.Any)

public abbrev Middleware := HandlerFn → HandlerFn

/-- Carries captured path parameters through request extensions. -/
public structure RouteParams where
  params : List (String × String)
deriving TypeName, Inhabited

public def RouteParams.lookup (self : RouteParams) (key : String) : Option String :=
  self.params.find? (·.1 == key) |>.map (·.2)

public structure Route where
  method     : Method
  pat        : RoutePattern
  handler    : HandlerFn
  middlewares : Array Middleware := #[]

/--
Appends a middleware to this route. Route middlewares run **after** router-level middlewares,
just before the handler.

Example:
```lean4
  GET "/admin" adminRoute (req : Request Body.Stream) := ...
  def router := Router.empty |>.addRoute (adminRoute.addMiddleware auth)
```
-/
public def Route.addMiddleware (middleware : Middleware) (self : Route) : Route :=
  { self with middlewares := self.middlewares.push middleware }

/--
Runtime route constructor. Prefer the `GET`/`POST`/... term macros for
compile-time pattern validation and extractor support.
Use this only when the handler is already built as a `HandlerFn`.
-/
public def Route.new (method : Method) (pattern : String) (handler : HandlerFn) : Route :=
  { method, handler, pat := RoutePattern.ofString pattern }

end LeanIO.Router
