module

public import Std.Http.Data.Request
public import Std.Http.Data.Headers.Value
public import Std.Async.Basic
public import LeanIO.Router
import LeanIO.Utils
public import LeanIO.Data.Redacted
import LeanIO.Data.Headers.HeaderName

namespace Std.Http.Header.Value

public def basicUnauthorized: Header.Value := mk "Basic realm=\"Restricted Area\""
public def bearerUnauthorized: Header.Value := mk "Bearer realm=\"Restricted Area\""

end Std.Http.Header.Value

namespace LeanIO.Middlewares
open LeanIO Router Utils
open Std Http Server
open Std.Async

public def extractAuthorization (request: Request α): Option String :=
  request.line.headers.get? .authorization |>.map (·.value)

public inductive AuthConfig where
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
public def auth (config: AuthConfig) : Middleware := fun next req => do
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
