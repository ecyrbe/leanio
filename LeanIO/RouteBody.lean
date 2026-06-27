import Lean
import Std.Http
import Std.Async

namespace LeanIO
open Std.Http
open Std.Async
open Lean


/--
Typeclass for parsing a request body into a typed value.

When a route handler declares a typed request parameter (not `Request Body.Stream`),
the body is parsed using `FromRouteBody.parse` before the handler runs.

Example:
```lean4
structure CreateTodoRequest where
  title  : String
  userId : Nat
deriving FromJson

POST "/todos" createTodo (req : Request CreateTodoRequest) := ...
-- the body stream is parsed via `FromRouteBody.parse : Body.Stream → Async (Except String CreateTodoRequest)`
```
-/
class FromRouteBody (α : Type) where
  parse : Body.Stream → Async (Except String α)

/--
Default instance: reads the body as a UTF-8 `String`, parses it as JSON,
then deserializes via `Lean.FromJson`.

To use, derive `FromJson` on your request type:

```lean4
structure CreateTodoRequest where
  title  : String
  userId : Nat
deriving FromJson
```
-/
instance {α : Type} [FromJson α] : FromRouteBody α where
  parse body := do
    let body : String ← body.readAll
    return (Lean.Json.parse body >>= fromJson? (α := α))

end LeanIO
