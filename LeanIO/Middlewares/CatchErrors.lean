import LeanIO.Router

namespace LeanIO.Middlewares
open LeanIO Router
open Std Http Server
open Std.Async

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
    (onError : IO.Error → ContextAsync (Response Body.Any) := fun _ => Response.internalServerError |>.empty):
    Middleware := fun next req => do
  try
    next req
  catch e =>
    onError e

end LeanIO.Middlewares
