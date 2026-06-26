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

/-- Validates route pattern structure: balanced braces and valid param names. -/
def validateRoutePattern (s : String) : Except String Unit :=
  Id.run do
    let mut chars := s.toList
    while !chars.isEmpty do
      match chars with
      | '{' :: rest =>
        let (nameChars, after) := rest.span (· ≠ '}')
        if after.isEmpty then
          return Except.error "unclosed brace in pattern"
        let name := String.ofList nameChars
        unless isValidParamName name do
          return Except.error s!"invalid path parameter name '{name}'"
        chars := after.tail
      | _ :: rest => chars := rest
      | [] => chars := []
    return Except.ok ()

/-- Returns each path parameter name from a pattern string like "/user/{id}".
Assumes the pattern is already validated. -/
def extractParamNames (s : String) : List String :=
  Id.run do
    let mut chars := s.toList
    let mut acc : List String := []
    while !chars.isEmpty do
      match chars with
      | '{' :: rest =>
        let (nameChars, after) := rest.span (· ≠ '}')
        let name := String.ofList nameChars
        acc := name :: acc
        chars := after.tail
      | _ :: rest => chars := rest
      | [] => chars := []
    return acc.reverse

syntax parenBinder := "(" ident ":" term ")"

private def expandRouteDef (methodName : Name) (pat : TSyntax `str) (name : TSyntax `ident)
    (bs : Array Syntax) (body : TSyntax `term) : MacroM Command := do
  let patStr := pat.getString

  match validateRoutePattern patStr with
  | .error e => Macro.throwErrorAt pat e
  | .ok () => pure ()

  let paramNames := extractParamNames patStr
  let n := paramNames.length
  let methodTerm := mkIdent methodName

  let handler : Term ←
    match bs.toList with
    | [] => pure body
    | reqBinder :: paramBinders =>
      let (reqId, reqTy) ← match reqBinder with
        | `(parenBinder| ($id:ident : $ty:term)) => pure (id, ty)
        | _ => Macro.throwError "invalid request binder"

      let isRawRequest := match reqTy with
        | `(Request Body.Stream) => true
        | _ => false

      if paramBinders.isEmpty then
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

        let vsId := mkIdent `vs

        let pairs := List.zip (List.range paramBinders.length) paramBinders
        let parsedBody ← pairs.foldrM (fun (i, b) inner =>
          match b with
          | `(parenBinder| ($id:ident : $ty:term)) => do
            let idxLit := Syntax.mkNatLit i
            `(parseParam (($vsId).toArray[$idxLit]!) fun ($id : $ty) => $inner)
          | _ => Macro.throwError "invalid binder"
        ) body

        if isRawRequest then
          `(fun ($reqId : $reqTy) =>
            let path := toString ($reqId).line.uri.path
            match compiled.matchPath path with
            | some $vsId:ident => $parsedBody
            | none => Response.notFound |>.text "route not found")
        else
          `(fun ($reqId : Request Body.Stream) =>
            let path := toString ($reqId).line.uri.path
            match compiled.matchPath path with
            | some $vsId:ident =>
              parseBody $reqId fun ($reqId : $reqTy) => $parsedBody
            | none => Response.notFound |>.text "route not found")

  `(def $name : Route :=
    let compiled := RoutePattern.ofString $pat
    { method := $methodTerm, pat := compiled, handler := $handler })

syntax "GET " str ident parenBinder* ":=" term : command
macro_rules | `(GET $pat:str $name:ident $bs:parenBinder* := $body:term) => expandRouteDef `Method.get pat name bs body

syntax "POST " str ident parenBinder* ":=" term : command
macro_rules | `(POST $pat:str $name:ident $bs:parenBinder* := $body:term) => expandRouteDef `Method.post pat name bs body

syntax "PUT " str ident parenBinder* ":=" term : command
macro_rules | `(PUT $pat:str $name:ident $bs:parenBinder* := $body:term) => expandRouteDef `Method.put pat name bs body

syntax "DELETE " str ident parenBinder* ":=" term : command
macro_rules | `(DELETE $pat:str $name:ident $bs:parenBinder* := $body:term) => expandRouteDef `Method.delete pat name bs body

syntax "PATCH " str ident parenBinder* ":=" term : command
macro_rules | `(PATCH $pat:str $name:ident $bs:parenBinder* := $body:term) => expandRouteDef `Method.patch pat name bs body

syntax "HEAD " str ident parenBinder* ":=" term : command
macro_rules | `(HEAD $pat:str $name:ident $bs:parenBinder* := $body:term) => expandRouteDef `Method.head pat name bs body

end Leanio.Router
