import Std.Async.ContextAsync
import LeanIO.Router.Route
import LeanIO.Request.FromRequestBody
import LeanIO.Request.FromRequestParts
import LeanIO.Response.IntoResponse

namespace LeanIO
open Std.Http Std.Async Lean

class HandlerAdapter (Fn : Type) where
  adapt :
    Fn → Router.HandlerSig

instance [IntoResponse R]: HandlerAdapter (Unit → R) where
  adapt handler _ := IntoResponse.into_response <| pure (handler ())

instance [IntoResponse R]: HandlerAdapter (Unit → ContextAsync R) where
  adapt handler _ :=
    IntoResponse.into_response <| handler ()

instance [FromRequestBody P₁] [IntoResponse R]: HandlerAdapter (P₁ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    match body with
    | .ok body => IntoResponse.into_response <| handler body
    | .error err => Response.badRequest |>.text err

instance [FromRequestParts P₁] [IntoResponse R] : HandlerAdapter (P₁ → ContextAsync R) where
  adapt handler req :=
    let part := FromRequestParts.from_request_parts req
    match part with
    | .ok part => IntoResponse.into_response <| handler part
    | .error err => Response.badRequest |>.text err

instance [FromRequestBody P₁] [FromRequestParts P₂] [IntoResponse R]:
  HandlerAdapter (P₁ →  P₂ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part := FromRequestParts.from_request_parts (α:=P₂) req
    match body, part with
    | .ok body, .ok part => IntoResponse.into_response <| handler body part
    | .error err, .ok _ => Response.badRequest |>.text err
    | .ok _, .error err => Response.badRequest |>.text err
    | .error err1, .error err2 => Response.badRequest |>.text (err1 ++ "\r\n" ++ err2)

instance [FromRequestParts P₁] [FromRequestParts P₂] [IntoResponse R] :
  HandlerAdapter (P₁ →  P₂ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    match part1, part2 with
    | .ok part1, .ok part2 => IntoResponse.into_response <| handler part1 part2
    | .error err1, .ok _ => Response.badRequest |>.text err1
    | .ok _, .error err2 => Response.badRequest |>.text err2
    | .error err1, .error err2 => Response.badRequest |>.text (err1 ++ "\r\n" ++ err2)

instance [FromRequestBody P₁] [FromRequestParts P₂] [FromRequestParts P₃] [IntoResponse R] :
  HandlerAdapter (P₁ →  P₂ → P₃ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part1 := FromRequestParts.from_request_parts (α:=P₂) req
    let part2 := FromRequestParts.from_request_parts (α:=P₃) req
    match body, part1, part2 with
    | .ok body, .ok part1, .ok part2 => IntoResponse.into_response <| handler body part1 part2
    | _,_,_ => Response.badRequest |>.empty

instance [FromRequestParts P₁] [FromRequestParts P₂] [FromRequestParts P₃] [IntoResponse R] :
  HandlerAdapter (P₁ →  P₂ → P₃ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    let part3 := FromRequestParts.from_request_parts (α:=P₃) req
    match part1, part2, part3 with
    | .ok part1, .ok part2, .ok part3 => IntoResponse.into_response <| handler part1 part2 part3
    | _,_,_ => Response.badRequest |>.empty

instance [FromRequestBody P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [IntoResponse R]:
  HandlerAdapter (P₁ →  P₂ → P₃ → P₄ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part1 := FromRequestParts.from_request_parts (α:=P₂) req
    let part2 := FromRequestParts.from_request_parts (α:=P₃) req
    let part3 := FromRequestParts.from_request_parts (α:=P₄) req
    match body, part1, part2, part3 with
    | .ok body, .ok part1, .ok part2, .ok part3 => IntoResponse.into_response <| handler body part1 part2 part3
    | _,_,_,_ => Response.badRequest |>.empty

instance [FromRequestParts P₁] [FromRequestParts P₂] [FromRequestParts P₃]  [FromRequestParts P₄] [IntoResponse R]:
  HandlerAdapter (P₁ →  P₂ → P₃ → P₄ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    let part3 := FromRequestParts.from_request_parts (α:=P₃) req
    let part4 := FromRequestParts.from_request_parts (α:=P₄) req
    match part1, part2, part3, part4 with
    | .ok part1, .ok part2, .ok part3, .ok part4 => IntoResponse.into_response <| handler part1 part2 part3 part4
    | _,_,_,_ => Response.badRequest |>.empty

end LeanIO
