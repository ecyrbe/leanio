import Lean
import Std.Async.ContextAsync
import Std.Internal.Parsec.ByteArray
import LeanIO.Request.FromRequestBody
import LeanIO.Data.Headers.MimeType

namespace LeanIO
open Std.Http Std.Async MimeType
open Std Internal Parsec ByteArray Char
/-!
# URL-encoded Form Body Extraction

`Form α` + `FromForm α` parse `application/x-www-form-urlencoded` bodies into typed structs.
For multipart forms, see `LeanIO.Request.MultiPartForm`.
-/

structure Form (α : Type) where
  value : α

class FromForm (α : Type) where
  fromForm : URI.Query → Except String α

instance : FromForm (Std.HashMap String String) where
  fromForm m := do
    return HashMap.ofArray <| m.filterMap (fun (k,v) =>
        match k,v with
        | k, some v =>
          match k.decode, v.decode with
          | some k, some v => some (k,v)
          | _,_ => none
        | _,_ => none)

/-- extracted from Lean Std library URI parser -/
private def parseQuery (config : URI.Config) : Parser URI.Query := do
  let queryBytes ←
    takeWhileAtMost (fun c => isQueryChar c ∨ c = '%'.toUInt8) config.maxQueryLength

  let some queryStr := String.fromUTF8? queryBytes.toByteArray
    | fail "invalid query string"

  if queryStr.isEmpty then
    return URI.Query.empty

  let rawPairs := queryStr.split '&'

  if rawPairs.length > config.maxQueryParams then
    fail s!"too many query parameters (limit: {config.maxQueryParams})"

  let pairs : Option URI.Query := rawPairs.foldM (init := URI.Query.empty) fun acc pair => do
    match pair.split '=' |>.toStringList with
    | [key] =>
      let key ← URI.EncodedQueryParam.fromString? key
      pure (acc.insertEncoded key none)
    | key :: value =>
      let key ← URI.EncodedQueryParam.fromString? key
      let value ← URI.EncodedQueryParam.fromString? (String.intercalate "=" value)
      pure (acc.insertEncoded key (some value))
    | [] => pure acc  -- unreachable: splitOn always returns at least one element

  if let some pairs := pairs then
    return pairs
  else
    fail "invalid query string"

instance : HasMimeTypes (Form α) where
  mimes? := some [MimeType.formUrlEncoded]

instance [FromForm α] : FromRequestBody (Form α) where
  from_request_body req := do
    match checkMimeTypes (Form α) req.line.headers with
    | .ok _ => pure ()
    | .error e => return .error (.bad_media_error e)

    let body : ByteArray ← req.body.readAll
    match parseQuery {} body.iter with
    | .success _ query =>
      match FromForm.fromForm query with
      | .ok v => return .ok { value := v }
      | .error e => return .error (.semantic_error e)
    | .error _ .eof => return .error (.syntax_error "Form Body is incomplete")
    | .error _ (.other e) => return .error (.syntax_error e)

end LeanIO
