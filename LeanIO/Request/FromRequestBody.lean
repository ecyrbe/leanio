import Lean
import Std.Async.ContextAsync

namespace LeanIO
open Std.Http Std.Async Lean

def applicationJson := Header.Value.mk "application/json"
def plainText := Header.Value.mk "text/plain"

class FromRequestBody (α : Type) where
  from_request_body : Request Body.Stream → ContextAsync (Except String α)


structure Json (α: Type) where
  body: α

instance [FromJson α] : FromRequestBody (Json α) where
  from_request_body req := do
    if !req.line.headers.hasEntry .contentType applicationJson then
      return .error s!"application/json content-type expected, received {req.line.headers.get! .contentType}"
    let body : String ← req.body.readAll
    match Lean.Json.parse body with
    | .ok json =>
      match FromJson.fromJson? json with
      | .ok obj => return .ok {body:=obj}
      | .error e => return .error e
    | .error e => return .error e

structure PlainText where
  body: String

instance : FromRequestBody PlainText where
  from_request_body req := do
    if !req.line.headers.hasEntry .contentType plainText then
      return .error s!"text/plain content-type expected, received {req.line.headers.get! .contentType}"
    let body : String ← req.body.readAll
    return .ok {body}

end LeanIO
