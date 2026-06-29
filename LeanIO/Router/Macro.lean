import Lean
import Std.Http
import LeanIO.Router.RoutePattern
import LeanIO.Router.Route
import LeanIO.Router.HandlerAdapter

namespace LeanIO.Router
open Std Http Server
open Std.Async
open Lean
open Lean.Macro

def isValidParamName (s : String) : Bool :=
  if s.isEmpty then false
  else
    let first := s.front
    (first.isAlpha || first == '_') && s.all fun c => c.isAlphanum || c == '_'

/-- Validates route pattern structure: balanced braces, valid param names,
and ensures `{*name}` rest params appear only as the last segment. -/
def validateRoutePattern (s : String) : Except String Unit := do
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
def extractParamNames (s : String) : List String :=
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

syntax extractorBinder := "(" term ":" term ")"

/-- Builds a precomputed `RoutePattern` term from a path pattern string. -/
private def mkRoutePatternTerm (path : String) : MacroM Term := do
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
private def resolveMethodCon (keyword : Name) : Name :=
  let s := keyword.toString
  if s == "BASELINECONTROL" then `Method.baselineControl
  else if s == "VERSIONCONTROL" then `Method.versionControl
  else Name.mkStr2 "Method" (s.toLower)

/--
Expands a route term like `GET "/user/{id}" (⟨id⟩: Path Nat) => body` into
a `Route` value with a precomputed pattern and handler.
-/
private def expandRouteTerm (methodName : Name) (pat : TSyntax `str)
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
      `(let fn (_ : Unit) : Std.Async.ContextAsync _ := $body; LeanIO.HandlerAdapter.adapt fn)
    | binders =>
      let ascribedBody : Term := ← `(($body : Std.Async.ContextAsync _))
      let lam ← binders.foldrM (fun b inner => do
        match b with
        | `(extractorBinder| ($pat:term : $ty:term)) =>
          `(fun ($pat : $ty) => $inner)
        | _ => Macro.throwErrorAt b "invalid extractor binder"
      ) ascribedBody
      `(let fn := $lam; LeanIO.HandlerAdapter.adapt fn)

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

macro_rules
  | `($method:method $pat:str $bs:extractorBinder* => $body:term) => do
    let some source := method.raw.reprint | Macro.throwErrorAt method "failed to read method keyword"
    let methodName := source.trimAscii |>.toString |> Name.mkSimple
    expandRouteTerm (resolveMethodCon methodName) pat bs body

end LeanIO.Router
