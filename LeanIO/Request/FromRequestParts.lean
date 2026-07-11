module

import Std.Async.ContextAsync
public import LeanIO.Router.Route

namespace LeanIO
open Std.Http Std.Async

public inductive FromRequestPartsError where
| syntax_error (msg: String) -- 400 bad request
| semantic_error (msg: String) -- 422 unprocessable entity
| io_error (e: IO.Error) -- 500 internal server error

public def FromRequestPartsError.toStatus : FromRequestPartsError → Status
  | .syntax_error _ => Status.badRequest
  | .semantic_error _ => Status.unprocessableEntity
  | .io_error _ => Status.internalServerError

public instance : ToString FromRequestPartsError where
  toString
  | .syntax_error msg => msg
  | .semantic_error msg => msg
  | .io_error e => e.toString

public instance : MonadLift (Except String) (Except FromRequestPartsError) where
  monadLift
    | .ok x => .ok x
    | .error e => .error (.semantic_error e)

public class FromRequestParts (α : Type) where
  from_request_parts : Request Body.Stream → Except FromRequestPartsError α

/-!
 BASIC raw extractors
-/

public instance : FromRequestParts Method where
  from_request_parts req := pure req.line.method

public instance : FromRequestParts Version where
  from_request_parts req := pure req.line.version

public instance : FromRequestParts Headers where
  from_request_parts req := pure req.line.headers

public instance : FromRequestParts RequestTarget where
  from_request_parts req := pure req.line.uri

public instance : FromRequestParts URI.Query where
  from_request_parts req := pure req.line.uri.query

public instance : FromRequestParts URI.Path where
  from_request_parts req := pure req.line.uri.path

public class FromPath (α : Type) where
  fromPath : Std.HashMap String String → Except String α

public instance: FromPath (Std.HashMap String String) where
  fromPath h := .ok h

public class FromString (α : Type) where
  parse: String → Except String α

public instance : FromString String where
  parse s := .ok s

public instance : FromString Nat where
  parse s := match s.toNat? with
    | some n => .ok n
    | none => .error s!"Impossible to parse {s} as a Nat"

public instance : FromString Int where
  parse s := match s.toInt? with
    | some n => .ok n
    | none => .error s!"Impossible to parse {s} as a Int"

public instance : FromString Bool where
  parse s := match s with
    | "true" => .ok true
    | "false" => .ok false
    | _ => .error s!"Impossible to parse {s} as a Bool"

public structure Path (α : Type) where
  value : α

public instance [FromPath α] : FromRequestParts (Path α) where
  from_request_parts req := do
    let some ext := (req.extensions.get Router.RouteParams) | .error <|.io_error "Extension for RouteParams not found"
    let params := Std.HashMap.ofList ext.params
    let value ← FromPath.fromPath params
    return {value}

-- when only one parameter
public instance [FromString α] : FromRequestParts (Path α) where
  from_request_parts req := do
    let some ext := (req.extensions.get Router.RouteParams) | .error <|.io_error "Extension for RouteParams not found"
    if ext.params.length != 1 then
      .error (.semantic_error "expected exactly one path parameter")
    let value ← FromString.parse ext.params[0]!.2
    return {value}

public instance [FromString α] [FromString β] : FromRequestParts (Path (α × β)) where
  from_request_parts req := do
    let some ext := (req.extensions.get Router.RouteParams) | .error <|.io_error "Extension for RouteParams not found"
    if ext.params.length != 2 then
      .error <|.semantic_error "expected exactly two path parameters"
    let a: α ← FromString.parse ext.params[0]!.2
    let b: β ← FromString.parse ext.params[1]!.2
    return {value := (a, b)}

public instance [FromString α] [FromString β] [FromString γ] : FromRequestParts (Path (α × β × γ)) where
  from_request_parts req := do
    let some ext := (req.extensions.get Router.RouteParams) | .error <|.io_error "Extension for RouteParams not found"
    if ext.params.length != 3 then
      .error <|.semantic_error "expected exactly three path parameters"
    let a: α ← FromString.parse ext.params[0]!.2
    let b: β ← FromString.parse ext.params[1]!.2
    let c: γ ← FromString.parse ext.params[2]!.2
    return {value := (a, b, c)}

public instance [FromString α] [FromString β] [FromString γ] [FromString δ] : FromRequestParts (Path (α × β × γ × δ )) where
  from_request_parts req := do
    let some ext := (req.extensions.get Router.RouteParams) | .error <|.io_error "Extension for RouteParams not found"
    if ext.params.length != 4 then
      .error <|.semantic_error "expected exactly four path parameters"
    let a: α ← FromString.parse ext.params[0]!.2
    let b: β ← FromString.parse ext.params[1]!.2
    let c: γ ← FromString.parse ext.params[2]!.2
    let d: δ ← FromString.parse ext.params[3]!.2
    return {value := (a, b, c, d)}

public instance [FromString α] [FromString β] [FromString γ] [FromString δ] [FromString ε] : FromRequestParts (Path (α × β × γ × δ × ε)) where
  from_request_parts req := do
    let some ext := (req.extensions.get Router.RouteParams) | .error <|.io_error "Extension for RouteParams not found"
    if ext.params.length != 5 then
      .error <|.semantic_error "expected exactly five path parameters"
    let a: α ← FromString.parse ext.params[0]!.2
    let b: β ← FromString.parse ext.params[1]!.2
    let c: γ ← FromString.parse ext.params[2]!.2
    let d: δ ← FromString.parse ext.params[3]!.2
    let e: ε ← FromString.parse ext.params[4]!.2
    return {value := (a, b, c, d, e)}

public structure Query (α : Type) where
  value: α

public class FromQuery (α : Type) where
  fromQuery : URI.Query → Except String α

public instance [FromQuery α] : FromRequestParts (Query α) where
  from_request_parts req := do
    let value ← FromQuery.fromQuery req.line.uri.query
    return {value}

end LeanIO
