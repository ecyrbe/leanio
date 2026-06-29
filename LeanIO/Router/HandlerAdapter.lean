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

-- 0 params sync
instance [IntoResponse R]: HandlerAdapter (Unit → R) where
  adapt handler _ := IntoResponse.into_response <| pure (handler ())

-- 0 params async
instance [IntoResponse R]: HandlerAdapter (Unit → ContextAsync R) where
  adapt handler _ :=
    IntoResponse.into_response <| handler ()

-- 1 param: body
instance [FromRequestBody P₁] [IntoResponse R]: HandlerAdapter (P₁ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    match body with
    | .ok body => IntoResponse.into_response <| handler body
    | .error err => Response.badRequest |>.text err

-- 1 param: part
instance [FromRequestParts P₁] [IntoResponse R] : HandlerAdapter (P₁ → ContextAsync R) where
  adapt handler req :=
    let part := FromRequestParts.from_request_parts req
    match part with
    | .ok part => IntoResponse.into_response <| handler part
    | .error err => Response.badRequest |>.text err

-- 2 params: body + 1 part
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

-- 2 params: all parts
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

-- 3 params: body + 2 parts
instance [FromRequestBody P₁] [FromRequestParts P₂] [FromRequestParts P₃] [IntoResponse R] :
  HandlerAdapter (P₁ →  P₂ → P₃ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part1 := FromRequestParts.from_request_parts (α:=P₂) req
    let part2 := FromRequestParts.from_request_parts (α:=P₃) req
    match body, part1, part2 with
    | .ok body, .ok part1, .ok part2 => IntoResponse.into_response <| handler body part1 part2
    | _,_,_ => Response.badRequest |>.empty

-- 3 params: all parts
instance [FromRequestParts P₁] [FromRequestParts P₂] [FromRequestParts P₃] [IntoResponse R] :
  HandlerAdapter (P₁ →  P₂ → P₃ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    let part3 := FromRequestParts.from_request_parts (α:=P₃) req
    match part1, part2, part3 with
    | .ok part1, .ok part2, .ok part3 => IntoResponse.into_response <| handler part1 part2 part3
    | _,_,_ => Response.badRequest |>.empty

-- 4 params: body + 3 parts
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

-- 4 params: all parts
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

-- 5 params: body + 4 parts
instance [FromRequestBody P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part1 := FromRequestParts.from_request_parts (α:=P₂) req
    let part2 := FromRequestParts.from_request_parts (α:=P₃) req
    let part3 := FromRequestParts.from_request_parts (α:=P₄) req
    let part4 := FromRequestParts.from_request_parts (α:=P₅) req
    match body, part1, part2, part3, part4 with
    | .ok body, .ok part1, .ok part2, .ok part3, .ok part4 =>
      IntoResponse.into_response <| handler body part1 part2 part3 part4
    | _,_,_,_,_ => Response.badRequest |>.empty

-- 5 params: all parts
instance [FromRequestParts P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    let part3 := FromRequestParts.from_request_parts (α:=P₃) req
    let part4 := FromRequestParts.from_request_parts (α:=P₄) req
    let part5 := FromRequestParts.from_request_parts (α:=P₅) req
    match part1, part2, part3, part4, part5 with
    | .ok part1, .ok part2, .ok part3, .ok part4, .ok part5 =>
      IntoResponse.into_response <| handler part1 part2 part3 part4 part5
    | _,_,_,_,_ => Response.badRequest |>.empty

-- 6 params: body + 5 parts
instance [FromRequestBody P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part1 := FromRequestParts.from_request_parts (α:=P₂) req
    let part2 := FromRequestParts.from_request_parts (α:=P₃) req
    let part3 := FromRequestParts.from_request_parts (α:=P₄) req
    let part4 := FromRequestParts.from_request_parts (α:=P₅) req
    let part5 := FromRequestParts.from_request_parts (α:=P₆) req
    match body, part1, part2, part3, part4, part5 with
    | .ok body, .ok part1, .ok part2, .ok part3, .ok part4, .ok part5 =>
      IntoResponse.into_response <| handler body part1 part2 part3 part4 part5
    | _,_,_,_,_,_ => Response.badRequest |>.empty

-- 6 params: all parts
instance [FromRequestParts P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    let part3 := FromRequestParts.from_request_parts (α:=P₃) req
    let part4 := FromRequestParts.from_request_parts (α:=P₄) req
    let part5 := FromRequestParts.from_request_parts (α:=P₅) req
    let part6 := FromRequestParts.from_request_parts (α:=P₆) req
    match part1, part2, part3, part4, part5, part6 with
    | .ok part1, .ok part2, .ok part3, .ok part4, .ok part5, .ok part6 =>
      IntoResponse.into_response <| handler part1 part2 part3 part4 part5 part6
    | _,_,_,_,_,_ => Response.badRequest |>.empty

-- 7 params: body + 6 parts
instance [FromRequestBody P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [FromRequestParts P₇] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → P₇ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part1 := FromRequestParts.from_request_parts (α:=P₂) req
    let part2 := FromRequestParts.from_request_parts (α:=P₃) req
    let part3 := FromRequestParts.from_request_parts (α:=P₄) req
    let part4 := FromRequestParts.from_request_parts (α:=P₅) req
    let part5 := FromRequestParts.from_request_parts (α:=P₆) req
    let part6 := FromRequestParts.from_request_parts (α:=P₇) req
    match body, part1, part2, part3, part4, part5, part6 with
    | .ok body, .ok part1, .ok part2, .ok part3, .ok part4, .ok part5, .ok part6 =>
      IntoResponse.into_response <| handler body part1 part2 part3 part4 part5 part6
    | _,_,_,_,_,_,_ => Response.badRequest |>.empty

-- 7 params: all parts
instance [FromRequestParts P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [FromRequestParts P₇] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → P₇ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    let part3 := FromRequestParts.from_request_parts (α:=P₃) req
    let part4 := FromRequestParts.from_request_parts (α:=P₄) req
    let part5 := FromRequestParts.from_request_parts (α:=P₅) req
    let part6 := FromRequestParts.from_request_parts (α:=P₆) req
    let part7 := FromRequestParts.from_request_parts (α:=P₇) req
    match part1, part2, part3, part4, part5, part6, part7 with
    | .ok part1, .ok part2, .ok part3, .ok part4, .ok part5, .ok part6, .ok part7 =>
      IntoResponse.into_response <| handler part1 part2 part3 part4 part5 part6 part7
    | _,_,_,_,_,_,_ => Response.badRequest |>.empty

-- 8 params: body + 7 parts
instance [FromRequestBody P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [FromRequestParts P₇] [FromRequestParts P₈] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → P₇ → P₈ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part1 := FromRequestParts.from_request_parts (α:=P₂) req
    let part2 := FromRequestParts.from_request_parts (α:=P₃) req
    let part3 := FromRequestParts.from_request_parts (α:=P₄) req
    let part4 := FromRequestParts.from_request_parts (α:=P₅) req
    let part5 := FromRequestParts.from_request_parts (α:=P₆) req
    let part6 := FromRequestParts.from_request_parts (α:=P₇) req
    let part7 := FromRequestParts.from_request_parts (α:=P₈) req
    match body, part1, part2, part3, part4, part5, part6, part7 with
    | .ok body, .ok part1, .ok part2, .ok part3, .ok part4, .ok part5, .ok part6, .ok part7 =>
      IntoResponse.into_response <| handler body part1 part2 part3 part4 part5 part6 part7
    | _,_,_,_,_,_,_,_ => Response.badRequest |>.empty

-- 8 params: all parts
instance [FromRequestParts P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [FromRequestParts P₇] [FromRequestParts P₈] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → P₇ → P₈ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    let part3 := FromRequestParts.from_request_parts (α:=P₃) req
    let part4 := FromRequestParts.from_request_parts (α:=P₄) req
    let part5 := FromRequestParts.from_request_parts (α:=P₅) req
    let part6 := FromRequestParts.from_request_parts (α:=P₆) req
    let part7 := FromRequestParts.from_request_parts (α:=P₇) req
    let part8 := FromRequestParts.from_request_parts (α:=P₈) req
    match part1, part2, part3, part4, part5, part6, part7, part8 with
    | .ok part1, .ok part2, .ok part3, .ok part4, .ok part5, .ok part6, .ok part7, .ok part8 =>
      IntoResponse.into_response <| handler part1 part2 part3 part4 part5 part6 part7 part8
    | _,_,_,_,_,_,_,_ => Response.badRequest |>.empty

-- 9 params: body + 8 parts
instance [FromRequestBody P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [FromRequestParts P₇] [FromRequestParts P₈] [FromRequestParts P₉] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → P₇ → P₈ → P₉ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part1 := FromRequestParts.from_request_parts (α:=P₂) req
    let part2 := FromRequestParts.from_request_parts (α:=P₃) req
    let part3 := FromRequestParts.from_request_parts (α:=P₄) req
    let part4 := FromRequestParts.from_request_parts (α:=P₅) req
    let part5 := FromRequestParts.from_request_parts (α:=P₆) req
    let part6 := FromRequestParts.from_request_parts (α:=P₇) req
    let part7 := FromRequestParts.from_request_parts (α:=P₈) req
    let part8 := FromRequestParts.from_request_parts (α:=P₉) req
    match body, part1, part2, part3, part4, part5, part6, part7, part8 with
    | .ok body, .ok part1, .ok part2, .ok part3, .ok part4, .ok part5, .ok part6, .ok part7, .ok part8 =>
      IntoResponse.into_response <| handler body part1 part2 part3 part4 part5 part6 part7 part8
    | _,_,_,_,_,_,_,_,_ => Response.badRequest |>.empty

-- 9 params: all parts
instance [FromRequestParts P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [FromRequestParts P₇] [FromRequestParts P₈] [FromRequestParts P₉] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → P₇ → P₈ → P₉ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    let part3 := FromRequestParts.from_request_parts (α:=P₃) req
    let part4 := FromRequestParts.from_request_parts (α:=P₄) req
    let part5 := FromRequestParts.from_request_parts (α:=P₅) req
    let part6 := FromRequestParts.from_request_parts (α:=P₆) req
    let part7 := FromRequestParts.from_request_parts (α:=P₇) req
    let part8 := FromRequestParts.from_request_parts (α:=P₈) req
    let part9 := FromRequestParts.from_request_parts (α:=P₉) req
    match part1, part2, part3, part4, part5, part6, part7, part8, part9 with
    | .ok part1, .ok part2, .ok part3, .ok part4, .ok part5, .ok part6, .ok part7, .ok part8, .ok part9 =>
      IntoResponse.into_response <| handler part1 part2 part3 part4 part5 part6 part7 part8 part9
    | _,_,_,_,_,_,_,_,_ => Response.badRequest |>.empty

-- 10 params: body + 9 parts
instance [FromRequestBody P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [FromRequestParts P₇] [FromRequestParts P₈] [FromRequestParts P₉] [FromRequestParts P₁₀] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → P₇ → P₈ → P₉ → P₁₀ → ContextAsync R) where
  adapt handler req := do
    let body ← FromRequestBody.from_request_body req
    let part1 := FromRequestParts.from_request_parts (α:=P₂) req
    let part2 := FromRequestParts.from_request_parts (α:=P₃) req
    let part3 := FromRequestParts.from_request_parts (α:=P₄) req
    let part4 := FromRequestParts.from_request_parts (α:=P₅) req
    let part5 := FromRequestParts.from_request_parts (α:=P₆) req
    let part6 := FromRequestParts.from_request_parts (α:=P₇) req
    let part7 := FromRequestParts.from_request_parts (α:=P₈) req
    let part8 := FromRequestParts.from_request_parts (α:=P₉) req
    let part9 := FromRequestParts.from_request_parts (α:=P₁₀) req
    match body, part1, part2, part3, part4, part5, part6, part7, part8, part9 with
    | .ok body, .ok part1, .ok part2, .ok part3, .ok part4, .ok part5, .ok part6, .ok part7, .ok part8, .ok part9 =>
      IntoResponse.into_response <| handler body part1 part2 part3 part4 part5 part6 part7 part8 part9
    | _,_,_,_,_,_,_,_,_,_ => Response.badRequest |>.empty

-- 10 params: all parts
instance [FromRequestParts P₁] [FromRequestParts P₂] [FromRequestParts P₃] [FromRequestParts P₄] [FromRequestParts P₅] [FromRequestParts P₆] [FromRequestParts P₇] [FromRequestParts P₈] [FromRequestParts P₉] [FromRequestParts P₁₀] [IntoResponse R] :
  HandlerAdapter (P₁ → P₂ → P₃ → P₄ → P₅ → P₆ → P₇ → P₈ → P₉ → P₁₀ → ContextAsync R) where
  adapt handler req := do
    let part1 := FromRequestParts.from_request_parts (α:=P₁) req
    let part2 := FromRequestParts.from_request_parts (α:=P₂) req
    let part3 := FromRequestParts.from_request_parts (α:=P₃) req
    let part4 := FromRequestParts.from_request_parts (α:=P₄) req
    let part5 := FromRequestParts.from_request_parts (α:=P₅) req
    let part6 := FromRequestParts.from_request_parts (α:=P₆) req
    let part7 := FromRequestParts.from_request_parts (α:=P₇) req
    let part8 := FromRequestParts.from_request_parts (α:=P₈) req
    let part9 := FromRequestParts.from_request_parts (α:=P₉) req
    let part10 := FromRequestParts.from_request_parts (α:=P₁₀) req
    match part1, part2, part3, part4, part5, part6, part7, part8, part9, part10 with
    | .ok part1, .ok part2, .ok part3, .ok part4, .ok part5, .ok part6, .ok part7, .ok part8, .ok part9, .ok part10 =>
      IntoResponse.into_response <| handler part1 part2 part3 part4 part5 part6 part7 part8 part9 part10
    | _,_,_,_,_,_,_,_,_,_ => Response.badRequest |>.empty

end LeanIO
