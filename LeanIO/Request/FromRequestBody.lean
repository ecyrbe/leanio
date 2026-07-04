import Lean
import Std.Async.ContextAsync
import LeanIO.Data.MimeType

namespace LeanIO
open Std.Http Std.Async Lean MimeType

class FromRequestBody (α : Type) where
  from_request_body : Request Body.Stream → ContextAsync (Except String α)

structure Json (α: Type) where
  body: α

instance: HasMimeTypes (Json α) where
  mimes? := some [MimeType.applicationJson]

instance [FromJson α] : FromRequestBody (Json α) where
  from_request_body req := do
    match checkMimeTypes (Json α) req.line.headers with
    | .ok _ => pure ()
    | .error e => return .error e
    let body : String ← req.body.readAll
    match Lean.Json.parse body with
    | .ok json =>
      match FromJson.fromJson? json with
      | .ok obj => return .ok {body:=obj}
      | .error e => return .error e
    | .error e => return .error e

structure PlainText where
  body: String

instance : HasMimeTypes PlainText where
  mimes? := some [MimeType.textPlain]

instance : FromRequestBody PlainText where
  from_request_body req := do
    match checkMimeTypes (PlainText) req.line.headers with
    | .ok _ => pure ()
    | .error e => return .error e
    let body : String ← req.body.readAll
    return .ok {body}

instance[FromRequestBody α] [HasMimeTypes α] [FromRequestBody β] [HasMimeTypes β]: FromRequestBody (α ⊕ β) where
  from_request_body req := do
    match checkMimeTypes α req.line.headers with
    | .ok _ =>
      match ← FromRequestBody.from_request_body (α:=α) req with
      | .ok a => return .ok (Sum.inl a)
      | .error e => return .error e
    | .error ea =>
      match checkMimeTypes β req.line.headers with
      | .ok _ =>
        match ← FromRequestBody.from_request_body (α:=β) req with
        | .ok b => return .ok (Sum.inr b)
        | .error e => return .error e
      | .error eb => return .error s!"{ea} or {eb}"

end LeanIO
