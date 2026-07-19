import LeanIO.Router

namespace LeanIO.Middlewares
open LeanIO Router
open Std Http Server
open Std.Async

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
def withExtension (α : Type) [TypeName α] (data : α) : Middleware := fun next req =>
  next { req with extensions := req.extensions.insert data }

end LeanIO.Middlewares
