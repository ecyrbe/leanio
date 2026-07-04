import Lean
import Std.Async.ContextAsync
import LeanIO.Request.FromRequestBody
import LeanIO.Data.MimeType

namespace LeanIO
open Std.Http Std.Async MimeType

/-!
# URL-encoded Form Body Extraction

`Form α` + `FromForm α` parse `application/x-www-form-urlencoded` bodies into typed structs.
For multipart forms, see `LeanIO.Request.MultiPartForm`.
-/

structure Form (α : Type) where
  value : α

class FromForm (α : Type) where
  fromForm : Std.HashMap String String → Except String α

instance : FromForm (Std.HashMap String String) where
  fromForm m := .ok m

private def pctDecode (s : String) : String :=
  let s := s.replace "+" " "
  let rec go (cs : List Char) (acc : List Char) : List Char :=
    match cs with
    | '%' :: a :: b :: rest =>
      let hexStr := String.ofList [a, b]
      match String.toNat? hexStr with
      | some n => go rest (Char.ofNat n :: acc)
      | none => go rest ('%' :: acc)
    | c :: rest => go rest (c :: acc)
    | [] => acc.reverse
  String.ofList (go s.toList [])

private def parseUrlEncoded (body : String) : Std.HashMap String String :=
  let pairs : List String := (body.split (fun c => c = '&')).toList.map (fun s => s.toString)
  pairs.foldl (init := (∅ : Std.HashMap String String)) fun map pair =>
    let kv : List String := (pair.split (fun c => c = '=')).toList.map (fun s => s.toString)
    match kv with
    | key :: val :: _ => map.insert (pctDecode key) (pctDecode val)
    | key :: _ => map.insert (pctDecode key) ""
    | _ => map

instance : HasMimeTypes (Form α) where
  mimes? := some [MimeType.formUrlEncoded]

instance [FromForm α] : FromRequestBody (Form α) where
  from_request_body req := do
    match checkMimeTypes (Form α) req.line.headers with
    | .ok _ => pure ()
    | .error e => return .error e

    let body : String ← req.body.readAll
    let map := parseUrlEncoded body
    match FromForm.fromForm map with
    | .ok v => return .ok { value := v }
    | .error e => return .error e

end LeanIO
