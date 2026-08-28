module

public import Lean.Data.Json
public meta import Lean.Elab.Deriving.FromToJson
public import LeanIO.Request.FromRequestBody
public import LeanIO.Response.IntoResponse
public import LeanIO.Response.BrowserCached

/-!
# `Lean.Data.Json` backend

Importing this module makes every type with a `Lean.ToJson`/`Lean.FromJson`
instance usable as a JSON request body or response, and brings the
`deriving ToJson, FromJson` handlers into scope.
-/

namespace LeanIO
open Std.Http

public def stdRender [Lean.ToJson α] (a : α) : String :=
  Lean.Json.pretty (Lean.toJson a)

public def stdDecode [Lean.FromJson α] (s : String) : Except JsonError α :=
  match Lean.Json.parse s with
  | .error e => .error (.syntax_error e)
  | .ok json =>
    match Lean.fromJson? (α := α) json with
    | .ok a => .ok a
    | .error e => .error (.semantic_error e)

public instance [Lean.ToJson α] : IntoResponse α :=
  .ofJson stdRender

public instance [Lean.ToJson α] : IntoResponse (Status × α) :=
  .ofJsonWithStatus stdRender

public instance [Lean.ToJson α] : IntoResponse (Status × Headers × α) :=
  .ofJsonWithStatusHeaders stdRender

public instance [Lean.ToJson α] : IntoResponseExt (BrowserCached α) :=
  .ofJson stdRender

public instance [Lean.FromJson α] : FromRequestBody (Json α) :=
  .ofJson stdDecode

end LeanIO
