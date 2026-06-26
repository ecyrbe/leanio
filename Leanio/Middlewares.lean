import Leanio.Router
import Leanio.Utils
open Leanio.Router
open Leanio.Utils
open Std Http Server
open Std.Async

namespace Leanio.Middlewares

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

end Leanio.Middlewares
