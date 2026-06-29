import Lean.Elab.Deriving.Basic
import Lean.Elab.Deriving.Util
import Lean.Meta.Inductive
import LeanIO.Request.FromRequestParts

namespace LeanIO

open Lean Elab Meta Command Deriving

private def mkFromPathBody (indVal : InductiveVal) : TermElabM Term := do
  let fields := getStructureFields (← getEnv) indVal.name
  let ctorVal ← getConstInfoCtor indVal.ctors.head!
  let numParams := indVal.numParams
  let ctorIdent := mkIdent ctorVal.name
  let fieldIdents : Array Ident := fields.map mkIdent

  forallTelescopeReducing ctorVal.type fun xs _ => do
    let mut stmts : Array (TSyntax `doElem) := #[]
    for h : i in [0:fields.size] do
      let fieldName  := fields[i]
      let fnameLit   := Syntax.mkStrLit (toString fieldName)
      let fident     := fieldIdents[i]!
      let x          := xs[numParams + i]!
      let tmpIdent   := mkIdent (Name.mkSimple s!"tmp_{i}")

      stmts := stmts.push (← `(doElem|
        let $tmpIdent:ident : String ← match h.get? $fnameLit:str with
          | some s => pure s
          | none => .error s!"missing path parameter '$fnameLit:str'"
      ))
      stmts := stmts.push (← `(doElem|
        let $fident:ident ← FromString.parse $tmpIdent:ident
      ))
    let fieldTerms : Array Term := fieldIdents.map fun i => i
    let result ← `(.ok ($ctorIdent:ident $fieldTerms:term*))
    `(fun h => do
      $[$stmts:doElem]*
      $result:term)

private def mkFromPathInstanceCmd (declName : Name) : TermElabM Command := do
  let indVal ← getConstInfoInduct declName
  let argNames ← mkInductArgNames indVal
  let binders ← mkImplicitBinders argNames
  let indType ← mkInductiveApp indVal argNames
  let type ← `($(mkCIdent ``FromPath) $indType)
  let instName ← mkInstName ``FromPath declName
  let body ← mkFromPathBody indVal

  `(instance $(mkIdent instName):ident $binders:implicitBinder* : $type := ⟨$body⟩)

open Command

/--
Derives a `FromPath` instance for a structure so that named path
parameters are deserialized into its fields by matching field names
against route parameter names. This enables `Path MyStruct` in handlers.

```lean4
structure UserParams where
  userId : Nat
  name   : String
deriving FromPath

-- usable as:
GET "/users/{userId}/{name}" (params : Path UserParams) => ...
```
-/
def mkFromPathInstanceHandler (declNames : Array Name) : CommandElabM Bool := do
  if (← declNames.allM isInductive) && declNames.size > 0 then
    for declName in declNames do
      if isStructure (← getEnv) declName then
        let instCmd ← liftTermElabM <| mkFromPathInstanceCmd declName
        elabCommand instCmd
    return true
  else
    return false

initialize
  registerDerivingHandler ``FromPath mkFromPathInstanceHandler

end LeanIO
