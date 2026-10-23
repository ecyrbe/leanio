module

public meta import Lean
import Std.Http
import LeanIO.Router.RoutePattern
import LeanIO.Router.Route
public import LeanIO.Router.Extractor
public import LeanIO.Backend.LeanIn

namespace LeanIO.Router
open Std Http Server
open Std.Async
open Lean
open Lean.Macro

/-- Selects the native backend for the unprefixed `GET … => …` route syntax in the current scope. The
default keeps the stock `ContextAsync` backend; `set_option leanio.backend.native true` switches it. -/
register_option leanio.backend.native : Bool := {
  defValue := false
  descr := "expand `GET … => …` and the other route keywords to the native LeanIn backend"
}

public meta def isValidParamName (s : String) : Bool :=
  if s.isEmpty then false
  else
    let first := s.front
    (first.isAlpha || first == '_') && s.all fun c => c.isAlphanum || c == '_'

/-- Validates route pattern structure: balanced braces, valid param names,
and ensures `{*name}` rest params appear only as the last segment. -/
public meta def validateRoutePattern (s : String) : Except String Unit := do
  unless s.startsWith "/" do
    throw "route pattern must start with '/'"
  let parts := s.split '/' |>.map toString |>.filter (¬ ·.isEmpty) |>.toList
  let hasRest := parts.any fun p => p.startsWith "{*" && p.endsWith "}"
  for p in parts do
    if p.startsWith '{' || p.endsWith '}' then
      unless p.startsWith '{' && p.endsWith '}' do
        throw "unclosed brace in pattern"
      let inner := p.drop 1 |>.dropEnd 1 |>.toString
      if inner.startsWith "*" then
        let name := inner.drop 1 |>.toString
        unless isValidParamName name do
          throw s!"invalid rest parameter name '{name}'"
      else
        unless isValidParamName inner do
          throw s!"invalid path parameter name '{inner}'"
  if hasRest then
    let lastPart := parts.getLast?.getD ""
    unless lastPart.startsWith "{*" && lastPart.endsWith "}" do
      let name := parts.find? (fun p => p.startsWith "{*" && p.endsWith "}")
        |>.getD ""
        |>.drop 2 |>.dropEnd 1 |>.toString
      throw s!"rest parameter '{name}' must be the last path segment"
  return ()

/-- Returns each path parameter name from a pattern string like "/user/{id}"
or "/files/{*path}". For rest params, the `*` is stripped from the name. -/
public meta def extractParamNames (s : String) : List String :=
  let parts := s.split '/' |>.map toString |>.filter (¬ ·.isEmpty) |>.toList
  parts.filterMap fun p =>
    if p.startsWith '{' && p.endsWith '}' then
      let inner := p.drop 1 |>.dropEnd 1 |>.toString
      if inner.startsWith "*" then
        some (inner.drop 1 |>.toString)
      else
        some inner
    else
      none

public syntax extractorBinder := "(" term ":" term ")"

/-- Builds a precomputed `RoutePattern` term from a path pattern string. -/
meta def mkRoutePatternTerm (path : String) : MacroM Term := do
  let parts := path.split '/'
    |>.map toString
    |>.filter (fun s => !s.isEmpty)
    |>.toList
  let hasRest := parts.any fun s => s.startsWith "{*" && s.endsWith "}"
  let segs : List Term := parts.map fun s : String =>
    if s.startsWith "{*" && s.endsWith "}" then
      let name := s.drop 2 |>.dropEnd 1 |>.toString
      Syntax.mkApp (mkIdent ``Segment.rest) #[Syntax.mkStrLit name]
    else if s.startsWith "{" && s.endsWith "}" then
      let name := s.drop 1 |>.dropEnd 1 |>.toString
      Syntax.mkApp (mkIdent ``Segment.param) #[Syntax.mkStrLit name]
    else
      Syntax.mkApp (mkIdent ``Segment.lit) #[Syntax.mkStrLit s]
  let nilTerm := Syntax.mkApp (mkIdent ``List.nil) #[]
  let mut listTerm := nilTerm
  for seg in segs.reverse do
    listTerm := Syntax.mkApp (mkIdent ``List.cons) #[seg, listTerm]
  let lenLit := Syntax.mkNumLit (toString parts.length)
  let hasRestLit := if hasRest then mkIdent `true else mkIdent `false
  `({ segments := $listTerm, length := $lenLit, hasRest := $hasRestLit : RoutePattern })

/--
Maps an uppercase method keyword (e.g. `GET`) to its `Method.` constructor name
(e.g. `Method.get`).  Most methods just lowercase the whole keyword; two
constructors (`baselineControl`, `versionControl`) are handled specially.
-/
public meta def resolveMethodCon (keyword : Name) : Name :=
  let s := keyword.toString
  let root := Name.str (Name.str (Name.str .anonymous "Std") "Http") "Method"
  if s == "BASELINECONTROL" then Name.str root "baselineControl"
  else if s == "VERSIONCONTROL" then Name.str root "versionControl"
  else Name.str root s.toLower

/--
Expands a route term like `GET "/user/{id}" (⟨id⟩: Path Nat) => body` into
a `Route` value with a precomputed pattern and handler.
-/
meta def expandRouteTerm (methodName : Name) (pat : TSyntax `str)
    (bs : Array Syntax) (body : TSyntax `term) : MacroM Term := do
  let patStr := pat.getString
  let patTerm ← mkRoutePatternTerm patStr

  match validateRoutePattern patStr with
  | .error e => Macro.throwErrorAt pat e
  | .ok () => pure ()

  let paramNames := extractParamNames patStr
  unless paramNames.isEmpty do
    let hasPath := bs.any fun b =>
      match b with
      | `(extractorBinder| ($_:term : Path $_)) => true
      | _ => false
    unless hasPath do
      Macro.throwErrorAt pat s!"pattern has path parameter(s) {paramNames} but no Path extractor in handler"

  let methodTerm := mkIdent methodName

  let handler : Term ←
    match bs.toList with
    | [] =>
      match body with
      | `(do $_) =>
        `(let fn (_ : Unit) : Std.Async.ContextAsync _ := $body; LeanIO.Extractor.extract fn)
      | _ =>
        `(let fn (_ : Unit) := $body; LeanIO.Extractor.extract fn)
    | binders =>
      let ascribedBody : Term := ← `(($body : Std.Async.ContextAsync _))
      let lam ← binders.foldrM (fun b inner => do
        match b with
        | `(extractorBinder| ($pat:term : $ty:term)) =>
          `(fun ($pat : $ty) => $inner)
        | _ => Macro.throwErrorAt b "invalid extractor binder"
      ) ascribedBody
      `(let fn := $lam; LeanIO.Extractor.extract fn)

  `({ method := $methodTerm, pat := $patTerm, handler := $handler : Route })

/--
All valid HTTP route method keywords.
-/
declare_syntax_cat method

syntax "GET"  : method
syntax "POST" : method
syntax "PUT"  : method
syntax "DELETE" : method
syntax "PATCH" : method
syntax "HEAD" : method
syntax "OPTIONS" : method
syntax "CONNECT" : method
syntax "TRACE" : method
syntax "PROPFIND" : method
syntax "PROPPATCH" : method
syntax "MKCOL" : method
syntax "COPY" : method
syntax "MOVE" : method
syntax "LOCK" : method
syntax "UNLOCK" : method
syntax "SEARCH" : method
syntax "ACL" : method
syntax "BIND" : method
syntax "REBIND" : method
syntax "UNBIND" : method
syntax "REPORT" : method
syntax "QUERY" : method
syntax "UPDATE" : method
syntax "LABEL" : method
syntax "LINK" : method
syntax "UNLINK" : method
syntax "CHECKIN" : method
syntax "CHECKOUT" : method
syntax "UNCHECKOUT" : method
syntax "MERGE" : method
syntax "MKACTIVITY" : method
syntax "MKCALENDAR" : method
syntax "MKREDIRECTREF" : method
syntax "MKWORKSPACE" : method
syntax "ORDERPATCH" : method
syntax "PRI" : method
syntax "BASELINECONTROL" : method
syntax "UPDATEREDIRECTREF" : method
syntax "VERSIONCONTROL" : method

/--
Route term macro. Expands to a `Route` value.

```lean4
def myRoute : Route := GET "/user/{id}" (req : Request Body.Stream) (id : Nat) => ...
router.addRoute (POST "/todos" (req : Request CreateTodoRequest) => ...)
```
-/
syntax method str extractorBinder* "=>" term : term

/-- A route value for the native backend, from an already-built handler: `nativeRoute GET "/ping" ping`. -/
syntax "nativeRoute" method str term : term

meta def expandNativeRouteValue (m : TSyntax `method) (pat : TSyntax `str) (body : TSyntax `term) :
    MacroM Term := do
  let some source := m.raw.reprint | Macro.throwErrorAt m "failed to read method keyword"
  let methodName := source.trimAscii |>.toString |> Name.mkSimple
  let methodTerm : Term := mkIdent (resolveMethodCon methodName)
  let patStr := pat.getString
  match validateRoutePattern patStr with
  | .error e => Macro.throwErrorAt pat e
  | .ok () => pure ()
  `({ method := $methodTerm, pattern := RoutePattern.ofString $pat, handler := $body :
      LeanIO.Backend.LeanIn.NativeRoute })

/-- The native expansion of the arrow route syntax: the body is an `AppM` computation and the binder chain is
adapted by the native extractor classes. -/
meta def expandNativeArrow (methodName : Name) (pat : TSyntax `str) (bs : Array Syntax)
    (body : TSyntax `term) : MacroM Term := do
  let patStr := pat.getString
  match validateRoutePattern patStr with
  | .error e => Macro.throwErrorAt pat e
  | .ok () => pure ()
  let patTerm ← mkRoutePatternTerm patStr
  let methodTerm : Term := mkIdent (resolveMethodCon methodName)
  let handler ←
    match bs.toList with
    | [] =>
      `(let fn (_ : Unit) : LeanIO.Backend.LeanIn.AppM Unit :=
          LeanIO.Backend.LeanIn.runBody $body
        LeanIO.Backend.LeanIn.NativeExtractor.extract fn)
    | binders =>
      let lam ← binders.foldrM (fun b inner => do
        match b with
        | `(extractorBinder| ($pat:term : $ty:term)) => `(fun ($pat : $ty) => $inner)
        | _ => Macro.throwErrorAt b "invalid extractor binder")
        (← `(LeanIO.Backend.LeanIn.runBody $body))
      `(let fn := $lam; LeanIO.Backend.LeanIn.NativeExtractor.extract fn)
  `({ method := $methodTerm, pattern := $patTerm, handler := $handler :
      LeanIO.Backend.LeanIn.NativeRoute })

/-- `native GET "/todos" (body : Json T) => …` — the route declaration syntax, selecting the native backend.
The unprefixed `GET … => …` remains the stock backend. -/
syntax "native " method str extractorBinder* "=>" term : term

elab_rules : term
  | `($method:method $pat:str $bs:extractorBinder* => $body:term) => do
    let some source := method.raw.reprint | throwErrorAt method "failed to read method keyword"
    let methodName := source.trimAscii |>.toString |> Name.mkSimple
    let useNative ← getBoolOption `leanio.backend.native
    let stx ← Lean.Elab.liftMacroM <|
      if useNative then expandNativeArrow methodName pat bs body
      else expandRouteTerm (resolveMethodCon methodName) pat bs body
    Lean.Elab.Term.elabTerm stx none

macro_rules
  | `(native $method:method $pat:str $bs:extractorBinder* => $body:term) => do
    let some source := method.raw.reprint | Macro.throwErrorAt method "failed to read method keyword"
    let methodName := source.trimAscii |>.toString |> Name.mkSimple
    expandNativeArrow methodName pat bs body
  | `(nativeRoute $method:method $pat:str $body:term) => expandNativeRouteValue method pat body

end LeanIO.Router
