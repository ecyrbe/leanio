import LeanIO.Router
import LeanIO.Utils
import LeanIO.Data.Redacted
import LeanIO.Data.Headers.HeaderName

namespace Std.Http.Header.Value

def basicUnauthorized: Header.Value := mk "Basic realm=\"Restricted Area\""
def bearerUnauthorized: Header.Value := mk "Bearer realm=\"Restricted Area\""

end Std.Http.Header.Value

namespace LeanIO.Middlewares
open LeanIO Router Utils
open Std Http Server
open Std.Async

/--
Logger Middleware that logs each request and the time to respond.

Example:
```lean4
  router.addMiddleware requestLogger
```
-/
def requestLogger (next : HandlerSig) : HandlerSig := fun req => do
  let path := toString req.line.uri.path
  let method := toString req.line.method
  let start ← IO.monoNanosNow
  IO.println s!"→ {method} {path}"
  let res ← next req
  let status := toString res.line.status
  let end_ ← IO.monoNanosNow
  IO.println s!"← {status} ({formatNanos (end_ - start)})"
  return res

def extractAuthorization (request: Request α): Option String :=
  request.line.headers.get? .authorization |>.map (·.value)

/--
Creates a middleware that injects `data` into every request's extensions.
Use with a wrapper struct that derives `TypeName`:

```lean4
structure Meta where
  metadata: String
deriving TypeName

Router.addMiddleware (withExtension Meta { metadata := "hello" }) router
```
-/
def withExtension (α : Type) [TypeName α] (data : α) (next : HandlerSig) : HandlerSig := fun req =>
  next { req with extensions := req.extensions.insert data }

inductive AuthConfig where
| basic (validate: String → Redacted → Async Bool)
| bearer (validate: Redacted → Async Bool)

/--
Catches errors thrown by the next handler and returns an internal server error by default.

A custom error handler can be provided to inspect the exception and return a different response.

Should be added **after** other middlewares to become the outermost wrapper.

Example:
```lean4
  router.addMiddleware requestLogger
    |>.addMiddleware (← todoMiddleware)
    |>.addMiddleware catchErrors
  -- or with a custom handler:
  router.addMiddleware <| catchErrors fun e => do
    IO.eprintln s!"caught: {e}"
    Response.serviceUnavailable |>.empty
```
-/
def catchErrors
    (onError : IO.Error → ContextAsync (Response Body.Any) := fun _ => Response.internalServerError |>.empty)
    (next : HandlerSig) : HandlerSig := fun req => do
  try
    next req
  catch e =>
    onError e

/--
auth middleware that support both basic and bearer authentication.
it delegates verification to an async predicate.

Example:
```lean4
  router.addMiddleware <| auth (.basic fun user pwd => pure true)
```
-/
def auth (config: AuthConfig) (next: HandlerSig) : HandlerSig := fun req => do
  let some headerAuth := extractAuthorization req |
    match config with
    | .basic _ =>  Response.unauthorized |>.header .wwwAuthenticate .basicUnauthorized  |>.empty
    | .bearer _ =>  Response.unauthorized |>.header .wwwAuthenticate .bearerUnauthorized |>.empty
  match config with
  | .basic validate =>
    match parseBasicAuth headerAuth with
    | some (user, pass) =>
          if ← validate user pass then
            next req
          else
            Response.forbidden |>.empty
    | none => Response.unauthorized |>.empty
  | .bearer validate =>
    match parseBearer headerAuth with
    | some token =>
          if ← validate token then
            next req
          else
            Response.forbidden |>.empty
    | none => Response.unauthorized |>.empty

end LeanIO.Middlewares
