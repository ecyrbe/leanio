import Std.Async.ContextAsync
import LeanIO.Router.Route
import LeanIO.Request.FromRequestParts
import LeanIO.Request.FromRequestBody
import LeanIO.Response.IntoResponse

namespace LeanIO
open Std.Http Std.Async

/--
Adapts a user-defined handler function into the router's internal `HandlerSig`.

A handler function can take up to one body parameter (extracted from the request
body, implementing `FromRequestBody`) followed by any number of parts parameters
(extracted from path, headers, query, or extensions, implementing `FromRequestParts`).
The handler must return a `ContextAsync R` where `R` implements `IntoResponse`.

```lean4
-- 0 params (sync)
GET "/ping" => pure "pong"

-- 0 params (async)
GET "/status" => do
  return Status.ok

-- 1 body param
POST "/todos" (body : Json CreateTodoRequest) => do
  ...
  return (Status.created, todo)

-- 1 parts param
GET "/todos/{id}" (id : Path Nat) => do
  ...
  return todo

-- body + parts
PUT "/todos/{id}" (body : Json UpdateTodoRequest) (id : Path Nat) => do
  ...
  return todo

-- only 1 body allowed, it must be the first parameter
```
-/
class HandlerAdapter (Fn : Type) where
  adapt : Fn → Router.HandlerSig

private class PartsAdapter (Fn : Type) where
  adaptParts : Fn → Router.HandlerSig

instance [IntoResponse R] : HandlerAdapter (ContextAsync R) where
  adapt handler _ := IntoResponse.into_response handler

instance [IntoResponse R] : HandlerAdapter (Unit → R) where
  adapt handler _ := IntoResponse.into_response <| pure (handler ())

instance [IntoResponse R] : HandlerAdapter (Unit → ContextAsync R) where
  adapt handler _ := IntoResponse.into_response (handler ())

instance [FromRequestBody P] [PartsAdapter Rest] : HandlerAdapter (P → Rest) where
  adapt handler req := do
    match ← FromRequestBody.from_request_body (α:=P) req with
    | .ok p => PartsAdapter.adaptParts (handler p) req
    | .error e => Response.badRequest |>.text e

instance [FromRequestParts P] [PartsAdapter Rest] : HandlerAdapter (P → Rest) where
  adapt handler req := do
    match FromRequestParts.from_request_parts (α:=P) req with
    | .ok p => PartsAdapter.adaptParts (handler p) req
    | .error e => Response.badRequest |>.text e

-- PartsAdapter: body already consumed. Only FromRequestParts allowed.

private instance [IntoResponse R] : PartsAdapter (ContextAsync R) where
  adaptParts handler _ := IntoResponse.into_response handler

private instance [FromRequestParts P] [PartsAdapter Rest] : PartsAdapter (P → Rest) where
  adaptParts handler req := do
    match FromRequestParts.from_request_parts (α:=P) req with
    | .ok p => PartsAdapter.adaptParts (handler p) req
    | .error e => Response.badRequest |>.text e

end LeanIO
