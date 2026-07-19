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

def extractAuthorization (request: Request α): Option String :=
  request.line.headers.get? .authorization |>.map (·.value)

inductive AuthConfig where
| basic (validate: String → Redacted → Async Bool)
| bearer (validate: Redacted → Async Bool)

/--
auth middleware that support both basic and bearer authentication.
it delegates verification to an async predicate.

Example:
```lean4
  router.addMiddleware <| auth (.basic fun user pwd => pure true)
```
-/
def auth (config: AuthConfig) : Middleware := fun next req => do
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
