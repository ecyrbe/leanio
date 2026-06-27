import Lean
import Std.Http
import Leanio.RouteParam
import Leanio.RouteBody
import Leanio.Router.RoutePattern
import Leanio.Router.Route

namespace Leanio.Router
open Std Http Server
open Std.Async
open Lean
open Lean.Macro

private def parseParam [FromRouteParam α] (v : String)
    (f : α → ContextAsync (Response Body.Any)) : ContextAsync (Response Body.Any) :=
  match FromRouteParam.parse v with
  | .ok v => f v
  | .error e => Response.badRequest |>.text e

private def parseBody [FromRouteBody α] (req : Request Body.Stream)
    (f : Request α → ContextAsync (Response Body.Any)) : ContextAsync (Response Body.Any) := do
  match (← FromRouteBody.parse req.body) with
  | .ok v => f { line := req.line, body := v, extensions := req.extensions }
  | .error e => Response.badRequest |>.text e

def isValidParamName (s : String) : Bool :=
  if s.isEmpty then false
  else
    let first := s.front
    (first.isAlpha || first == '_') && s.all fun c => c.isAlphanum || c == '_'

/-- Validates route pattern structure: balanced braces and valid param names.
Accepts `{name}` for single-segment params and `{*name}` for rest/catch-all params
(which must be the last segment). -/
def validateRoutePattern (s : String) : Except String Unit :=
  Id.run do
    let parts := s.split '/' |>.map toString |>.filter (¬ ·.isEmpty) |>.toList
    for p in parts do
      if p.startsWith '{' || p.endsWith '}' then
        unless p.startsWith '{' && p.endsWith '}' do
          return Except.error "unclosed brace in pattern"
        let inner := p.drop 1 |>.dropEnd 1 |>.toString
        if inner.startsWith "*" then
          let name := inner.drop 1 |>.toString
          unless isValidParamName name do
            return Except.error s!"invalid rest parameter name '{name}'"
        else
          unless isValidParamName inner do
            return Except.error s!"invalid path parameter name '{inner}'"
    return Except.ok ()

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

syntax parenBinder := "(" ident ":" term ")"

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
Expands a route term like `GET "/user/{id}" (req) (id : Nat) => body` into
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
  let n := paramNames.length
  let methodTerm := mkIdent methodName

  let handler : Term ←
    match bs.toList with
    | [] =>
      if n ≠ 0 then
        Macro.throwErrorAt pat s!"pattern has {n} path parameter(s) but handler has none"
      pure body
    | reqBinder :: paramBinders =>
      let (reqId, reqTy) ← match reqBinder with
        | `(parenBinder| ($id:ident : $ty:term)) => pure (id, ty)
        | _ => Macro.throwError "invalid request binder"

      let isRawRequest := match reqTy with
        | `(Request Body.Stream) => true
        | _ => false

      if paramBinders.isEmpty then
        if n ≠ 0 then
          Macro.throwErrorAt pat s!"pattern has {n} path parameter(s) but handler has none"
        if isRawRequest then
          `(fun ($reqId : $reqTy) => $body)
        else
          `(fun ($reqId : Request Body.Stream) =>
            parseBody $reqId fun ($reqId : $reqTy) => $body)
      else
        if paramBinders.length ≠ n then
          Macro.throwErrorAt pat s!"handler has {paramBinders.length} parameter(s) but pattern has {n} path parameter(s)"

        for (expected, b) in List.zip paramNames paramBinders do
          match b with
          | `(parenBinder| ($id:ident : $_ty:term)) =>
            let actual := id.getId.toString
            unless actual == expected do
              Macro.throwErrorAt b s!"parameter '{actual}' does not match path parameter '{expected}'"
          | _ => Macro.throwError "invalid binder syntax"

        let paramsId := mkIdent `params

        let pairs := List.zip paramNames paramBinders
        let parsedBody ← pairs.foldrM (fun (name, b) inner =>
          match b with
          | `(parenBinder| ($id:ident : $ty:term)) => do
            let nameLit := Syntax.mkStrLit name
            `(parseParam ($paramsId[$nameLit]!) fun ($id : $ty) => $inner)
          | _ => Macro.throwError "invalid binder"
        ) body

        if isRawRequest then
          `(fun ($reqId : $reqTy) =>
            let $paramsId:ident := (($reqId).extensions.get Leanio.Router.RouteParams).getD { params := ∅ } |>.params
            $parsedBody)
        else
          `(fun ($reqId : Request Body.Stream) =>
            let $paramsId:ident := (($reqId).extensions.get Leanio.Router.RouteParams).getD { params := ∅ } |>.params
            parseBody $reqId fun ($reqId : $reqTy) => $parsedBody)

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
syntax method str parenBinder* "=>" term : term

macro_rules
  | `($method:method $pat:str $bs:parenBinder* => $body:term) => do
    let some source := method.raw.reprint | Macro.throwErrorAt method "failed to read method keyword"
    let methodName := source.trimAscii |>.toString |> Name.mkSimple
    expandRouteTerm (resolveMethodCon methodName) pat bs body

end Leanio.Router
