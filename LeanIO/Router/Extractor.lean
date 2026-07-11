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
The handler must return a `ContextAsync R` where `R` implements `IntoResponse` or `IntoResponseExt`.

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
class Extractor (Fn : Type) where
  extract : Fn → Router.HandlerSig

private class PartsExtractor (Fn : Type) where
  extractParts : Fn → Router.HandlerSig

instance [IntoResponse R] : Extractor (ContextAsync R) where
  extract handler _ := IntoResponse.into_response handler

instance [IntoResponseExt R] : Extractor (ContextAsync R) where
  extract handler req := IntoResponseExt.into_response_ext req handler

instance [IntoResponse R] : Extractor (Unit → R) where
  extract handler _ := IntoResponse.into_response <| pure (handler ())

instance [IntoResponseExt R] : Extractor (Unit → R) where
  extract handler req := IntoResponseExt.into_response_ext req <| pure (handler ())

instance [IntoResponse R] : Extractor (Unit → ContextAsync R) where
  extract handler _ := IntoResponse.into_response (handler ())

instance [IntoResponseExt R] : Extractor (Unit → ContextAsync R) where
  extract handler req := IntoResponseExt.into_response_ext req (handler ())

instance [FromRequestBody P] [PartsExtractor Rest] : Extractor (P → Rest) where
  extract handler req := do
    match ← FromRequestBody.from_request_body (α:=P) req with
    | .ok p => PartsExtractor.extractParts (handler p) req
    | .error e => Response.new |>.status e.toStatus |>.text (toString e)

instance [FromRequestParts P] [PartsExtractor Rest] : Extractor (P → Rest) where
  extract handler req := do
    match FromRequestParts.from_request_parts (α:=P) req with
    | .ok p => PartsExtractor.extractParts (handler p) req
    | .error e => Response.new |>.status e.toStatus |>.text (toString e)

-- PartsAdapter: body already consumed. Only FromRequestParts allowed.

private instance [IntoResponse R] : PartsExtractor (ContextAsync R) where
  extractParts handler _ := IntoResponse.into_response handler

private instance [IntoResponseExt R] : PartsExtractor (ContextAsync R) where
  extractParts handler req := IntoResponseExt.into_response_ext req handler

private instance [FromRequestParts P] [PartsExtractor Rest] : PartsExtractor (P → Rest) where
  extractParts handler req := do
    match FromRequestParts.from_request_parts (α:=P) req with
    | .ok p => PartsExtractor.extractParts (handler p) req
    | .error e => Response.new |>.status e.toStatus |>.text (toString e)

end LeanIO
