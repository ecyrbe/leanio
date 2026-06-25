import Lean
import Std.Http
import Std.Async

namespace Leanio
open Std.Http
open Std.Async
open Lean


class FromRouteBody (α : Type) where
  parse : Body.Stream → Async (Except String α)

instance {α : Type} [FromJson α] : FromRouteBody α where
  parse body := do
    let body : String ← body.readAll
    return (Lean.Json.parse body >>= fromJson? (α := α))

end Leanio
