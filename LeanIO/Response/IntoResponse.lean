module

public import Std.Http
public import Std.Async.ContextAsync
public import Lean.Data.Json

namespace LeanIO
open Std.Http Std.Async Lean

public class IntoResponse (α : Type) where
  into_response : ContextAsync α → ContextAsync (Response Body.Any)

public class IntoResponseExt (α : Type) where
  into_response_ext : Request Body.Stream → ContextAsync α → ContextAsync (Response Body.Any)

public instance : IntoResponse (Response Body.Any) where
  into_response resp := resp

public instance : IntoResponse Unit where
  into_response _ := Response.ok |>.empty

public instance : IntoResponse String where
  into_response str := do
    let str ← str
    Response.ok |>.text str

public instance : IntoResponse IO.Error where
  into_response err := do
    let err ← err
    Response.internalServerError |>.text err.toString

public instance : IntoResponse Status where
  into_response status := do
    let status ← status
    Response.new.status status |>.empty

public instance [ToJson α] : IntoResponse α where
  into_response a := do
    let a ← a
    Response.ok |>.json <| Json.pretty <| toJson a

public instance [IntoResponse ε] [IntoResponse α] : IntoResponse (Except ε α) where
  into_response res := do match ← res with
    | .ok ok => IntoResponse.into_response <| pure ok
    | .error e => IntoResponse.into_response <| pure e

public instance : IntoResponse (Status × String)  where
  into_response sstr := do
    let (s, str) ← sstr
    Response.new.status s |>.text str

public instance [ToJson α] : IntoResponse (Status × α)  where
  into_response sa := do
    let (s, a) ← sa
    Response.new.status s |>.json <| Json.pretty <| toJson a

public instance [ToJson α] : IntoResponse (Status × Headers × α)  where
  into_response sha := do
    let (s, h, a) ← sha
    Response.new.status s |>.headers h |>.json <| Json.pretty <| toJson a

end LeanIO
