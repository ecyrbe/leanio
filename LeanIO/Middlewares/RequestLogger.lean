import LeanIO.Router
import LeanIO.Utils

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
def requestLogger: Middleware := fun next req => do
  let path := toString req.line.uri.path
  let method := toString req.line.method
  let start ← IO.monoNanosNow
  IO.println s!"→ {method} {path}"
  let res ← next req
  let status := toString res.line.status
  let end_ ← IO.monoNanosNow
  IO.println s!"← {status} ({formatNanos (end_ - start)})"
  return res

end LeanIO.Middlewares
