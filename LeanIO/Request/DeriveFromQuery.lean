import Lean.Elab.Deriving.Basic
import Lean.Elab.Deriving.Util
import Lean.Meta.Inductive
import LeanIO.Request.FromRequestParts

namespace LeanIO

open Lean Elab Meta Command Deriving

private def mkFromQueryBody (indVal : InductiveVal) : TermElabM Term := do
  let env ← getEnv
  let fields := getStructureFields env indVal.name
  let ctorVal ← getConstInfoCtor indVal.ctors.head!
  let numParams := indVal.numParams
  let fieldIdents : Array Ident := fields.map mkIdent

  forallTelescopeReducing ctorVal.type fun xs _ => do
    let mut stmts : Array (TSyntax `doElem) := #[]

    for h : i in [0:fields.size] do
      let fieldName  := fields[i]
      let fnameLit   := Syntax.mkStrLit (toString fieldName)
      let fident     := fieldIdents[i]!
      let x          := xs[numParams + i]!
      let rawIdent   := mkIdent (Name.mkSimple s!"raw_{i}")

      let fieldType ← whnf (← inferType x)
      let isOpt := fieldType.isAppOfArity ``Option 1
      let defaultFn? := getDefaultFnForField? (← getEnv) indVal.name fieldName

      stmts := stmts.push (← `(doElem|
        let $rawIdent:ident : Option String := qs.get $fnameLit:str
      ))

      if isOpt then
        stmts := stmts.push (← `(doElem|
          let $fident:ident ← match $rawIdent:ident with
            | some s => Except.map Option.some <| FromString.parse s
            | none => .ok none
        ))
      else if let some defaultFn := defaultFn? then
        let defaultInfo ← getConstInfoDefn defaultFn
        let defaultTerm ← PrettyPrinter.delab defaultInfo.value
        stmts := stmts.push (← `(doElem|
          let $fident:ident ← match $rawIdent:ident with
            | some s => FromString.parse s
            | none => .ok $defaultTerm:term
        ))
      else
        let errLit := Syntax.mkStrLit ("missing query parameter '" ++ toString fieldName ++ "'")
        stmts := stmts.push (← `(doElem|
          let $fident:ident ← match $rawIdent:ident with
            | some s => FromString.parse s
            | none => .error $errLit:str
        ))

    let fieldTermIdents : Array Term := fieldIdents.map fun i => i
    let result ← `(.ok { $[$fieldIdents:ident := $fieldTermIdents:term],* })
    `(fun qs => do
      $[$stmts:doElem]*
      $result:term)

private def mkFromQueryInstanceCmd (declName : Name) : TermElabM Command := do
  let indVal ← getConstInfoInduct declName
  let argNames ← mkInductArgNames indVal
  let binders ← mkImplicitBinders argNames
  let indType ← mkInductiveApp indVal argNames
  let type ← `($(mkCIdent ``FromQuery) $indType)
  let instName ← mkInstName ``FromQuery declName
  let body ← mkFromQueryBody indVal

  `(instance $(mkIdent instName):ident $binders:implicitBinder* : $type := ⟨$body⟩)

/--
Derives a `FromQuery` instance for a structure so that query parameters
are deserialized into its fields by matching field names against query keys.
Fields with default values (e.g. `Nat := 0`) use the default when the key
is missing. `Option T` fields default to `none`. Other fields produce an
error if the key is missing.

```lean
structure Pagination where
  offset : Nat := 0
  limit  : Nat := 10
deriving FromQuery
```
-/
def mkFromQueryInstanceHandler (declNames : Array Name) : CommandElabM Bool := do
  if (← declNames.allM isInductive) && declNames.size > 0 then
    for declName in declNames do
      if isStructure (← getEnv) declName then
        let instCmd ← liftTermElabM <| mkFromQueryInstanceCmd declName
        elabCommand instCmd
    return true
  else
    return false

initialize
  registerDerivingHandler ``FromQuery mkFromQueryInstanceHandler

end LeanIO
