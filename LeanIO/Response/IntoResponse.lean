import Lean
import Std.Async.ContextAsync

namespace LeanIO
open Std.Http Std.Async Lean

class IntoResponse (α : Type) where
  into_response : ContextAsync α → ContextAsync (Response Body.Any)

instance : IntoResponse Unit where
  into_response _ := Response.ok |>.empty

instance : IntoResponse String where
  into_response str := do
    let str ← str
    Response.ok |>.text str

instance : IntoResponse IO.Error where
  into_response err := do
    let err ← err
    Response.internalServerError |>.text err.toString

instance : IntoResponse Status where
  into_response status := do
    let status ← status
    Response.new.status status |>.empty

instance [ToJson α] : IntoResponse α where
  into_response a := do
    let a ← a
    Response.ok |>.json <| Json.pretty <| toJson a

instance [IntoResponse ε] [IntoResponse α] : IntoResponse (Except ε α) where
  into_response res := do match ← res with
    | .ok ok => IntoResponse.into_response <| pure ok
    | .error e => IntoResponse.into_response <| pure e

instance [ToJson α] : IntoResponse (Status × α)  where
  into_response sa := do
    let (s, a) ← sa
    Response.new.status s |>.json <| Json.pretty <| toJson a

end LeanIO
