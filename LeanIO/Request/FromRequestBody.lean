import Lean
import Std.Async.ContextAsync
import LeanIO.Data.Headers.MimeType

namespace LeanIO
open Std.Http Std.Async Lean MimeType

inductive FromRequestBodyError where
| bad_media_error (msg: String) -- 415 unsupported media type
| syntax_error (msg: String) -- 400 bad request
| semantic_error (msg: String) -- 422 unprocessable entity
| io_error (e: IO.Error) -- 500 internal server error

def FromRequestBodyError.toStatus : FromRequestBodyError → Status
  | .bad_media_error _ => Status.unsupportedMediaType
  | .syntax_error _ => Status.badRequest
  | .semantic_error _ => Status.unprocessableEntity
  | .io_error _ => Status.internalServerError

instance : ToString FromRequestBodyError where
  toString
  | .bad_media_error msg => msg
  | .syntax_error msg => msg
  | .semantic_error msg => msg
  | .io_error e => e.toString

class FromRequestBody (α : Type) where
  from_request_body : Request Body.Stream → ContextAsync (Except FromRequestBodyError α)

structure Json (α: Type) where
  body: α

instance: HasMimeTypes (Json α) where
  mimes? := some [MimeType.applicationJson]

instance [FromJson α] : FromRequestBody (Json α) where
  from_request_body req := do
    match checkMimeTypes (Json α) req.line.headers with
    | .error e => return .error (.bad_media_error e)
    | .ok _ =>
      try
        let body : String ← req.body.readAll
        match Lean.Json.parse body with
        | .ok json =>
          match FromJson.fromJson? json with
          | .ok obj => return .ok {body:=obj}
          | .error e => return .error (.semantic_error e)
        | .error e => return .error (.syntax_error e)
      catch e => return .error (.io_error e)

structure PlainText where
  body: String

instance : HasMimeTypes PlainText where
  mimes? := some [MimeType.textPlain]

instance : FromRequestBody PlainText where
  from_request_body req := do
    match checkMimeTypes PlainText req.line.headers with
    | .error e => return .error (.bad_media_error e)
    | .ok _ =>
      try
        let body : String ← req.body.readAll
        return .ok {body}
      catch e => return .error (.io_error e)

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
      | .error eb => return .error (.bad_media_error s!"{ea} or {eb}")

end LeanIO
