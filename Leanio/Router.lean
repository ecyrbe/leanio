import Lean
import Std.Http
import Leanio.Utils
open Std Http Server
open Std.Async
open Leanio.Utils

namespace Leanio.Router

abbrev HandlerSig := Request Body.Stream → ContextAsync (Response Body.Any)

class FromRouteParam (α : Type) where
  parse : String → Except String α

instance : FromRouteParam String where
  parse s := .ok s

instance : FromRouteParam Nat where
  parse s := match s.toNat? with
    | some n => .ok n
    | none   => .error s!"cannot parse path param as Nat: {s}"

instance : FromRouteParam Int where
  parse s := match s.toInt? with
    | some n => .ok n
    | none   => .error s!"cannot parse path param as Int: {s}"

instance: FromRouteParam Bool where
  parse s := match s with
   | "true" => .ok true
   | "false" => .ok false
   | _ => .error s!"cannot parse path param as Bool: {s}"

def splitPath (path : String) : List String :=
  (path.split '/').toList.map toString |>.filter (· ≠ "")

structure RoutePattern where
  parts : List (Sum String String)

structure Route where
  method     : Method
  pat        : RoutePattern
  handler    : HandlerSig
  middlewares : List (HandlerSig → HandlerSig) := []

def parsePattern (path : String) : RoutePattern :=
  { parts := splitPath path |>.map fun s =>
    if s.startsWith "{" && s.endsWith "}" then
      Sum.inr (s.drop 1 |>.dropEnd 1 |>.toString)
    else
      Sum.inl s }

private partial def matchImpl
  (pat : List (Sum String String)) (seg : List String) : Option (List String) :=
  match pat, seg with
  | [], [] => some []
  | Sum.inl lit :: pat, s :: seg =>
    if lit == s then matchImpl pat seg else none
  | Sum.inr _ :: pat, s :: seg =>
    matchImpl pat seg |>.map fun rest => s :: rest
  | _, _ => none

def matchPath (pattern : RoutePattern) (path : String) : Option (List String) :=
  matchImpl pattern.parts (splitPath path)

def stripPathPrefix (full : String) (pre : String) : Option String :=
  let pSegs := splitPath pre
  let rSegs := splitPath full
  if pSegs.length > rSegs.length then none
  else
    let (given, remaining) := rSegs.splitAt pSegs.length
    if given = pSegs then
      some (if remaining.isEmpty then "/" else "/" ++ String.intercalate "/" remaining)
    else none

structure Router where
  routes      : List Route
  routers     : List (String × Router) := []
  middlewares : List (HandlerSig → HandlerSig) := []

def applyMiddlewares (ms : List (HandlerSig → HandlerSig)) (h : HandlerSig) : HandlerSig :=
  ms.foldl (fun h mw => mw h) h

partial def findRoute (router : Router) (methodRef : Method) (path : String) : Option HandlerSig := do
  for r in router.routes do
    if r.method = methodRef then
      match matchPath r.pat path with
      | some _ => return applyMiddlewares r.middlewares r.handler
      | none   => pure ()
  none

partial def dispatch (router : Router) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  let path := toString req.line.uri.path
  match findRoute router req.line.method path with
  | some h =>
    let wrapped := applyMiddlewares router.middlewares h
    wrapped req
  | none   =>
    let mut result : Option (ContextAsync (Response Body.Any)) := none
    for (pre, sub) in router.routers do
      match stripPathPrefix path pre with
      | some remaining =>
        let modifiedReq : Request Body.Stream :=
          { req with line := { req.line with uri := RequestTarget.parse! remaining } }
        let handler : HandlerSig := fun _ => dispatch sub modifiedReq
        let wrapped := applyMiddlewares router.middlewares handler
        result := some (wrapped req); break
      | none => pure ()
    match result with
    | some r => r
    | none => Response.notFound |>.text s!"404 Not Found: {req.line.method} {path}"

def loggingMiddleware (next : HandlerSig) : HandlerSig := fun req => do
  let path := toString req.line.uri.path
  let method := toString req.line.method
  let start ← IO.monoNanosNow
  IO.println s!"→ {method} {path}"
  let res ← next req
  let status := toString res.line.status
  let end_ ← IO.monoNanosNow
  IO.println s!"← {status} ({Utils.formatNanos (end_ - start)})"
  return res

instance : Handler Router where
  onRequest := dispatch

-- ==========================================
-- Inline route generation (macro)
-- ==========================================

private def parseParam [FromRouteParam α] (v : String)
    (f : α → ContextAsync (Response Body.Any)) : ContextAsync (Response Body.Any) :=
  match FromRouteParam.parse v with
  | .ok v => f v
  | .error e => Response.badRequest |>.text e

open Lean
open Lean.Macro

partial def extractParamNames (s : String) : List String :=
  go s.toList []
where
  go : List Char → List String → List String
    | [], acc => acc.reverse
    | '{' :: rest, acc =>
      let (name, after) := rest.span (· ≠ '}')
      go after.tail (String.ofList name :: acc)
    | _ :: rest, acc => go rest acc

syntax parenBinder := "(" ident ":" term ")"

private def expandRouteDef (methodName : Name) (pat : TSyntax `str) (name : TSyntax `ident)
    (bs : Array Syntax) (body : TSyntax `term) : MacroM Command := do
  let paramNames := extractParamNames pat.getString
  let n := paramNames.length
  let methodTerm := mkIdent methodName

  let handler : Term ←
    match bs.toList with
    | [] => pure body
    | reqBinder :: paramBinders =>
      let (reqId, reqTy) ← match reqBinder with
        | `(parenBinder| ($id:ident : $ty:term)) => pure (id, ty)
        | _ => Macro.throwError "invalid request binder"

      if paramBinders.isEmpty then
        `(fun ($reqId : $reqTy) => $body)
      else
        if paramBinders.length ≠ n then
          Macro.throwError s!"handler has {paramBinders.length} parameter(s) but pattern has {n} path parameter(s)"

        -- validate that binder names match path parameter names
        for (expected, b) in List.zip paramNames paramBinders do
          match b with
          | `(parenBinder| ($id:ident : $_ty:term)) =>
            let actual := id.getId.toString
            unless actual == expected do
              Macro.throwError s!"parameter '{actual}' does not match path parameter '{expected}' in pattern '{pat.getString}'"
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

        `(fun ($reqId : $reqTy) =>
          let path := toString ($reqId).line.uri.path
          match matchPath compiled path with
          | some $vsId:ident => $parsedBody
          | none => Response.notFound |>.text "route not found")

  `(def $name : Route :=
    let compiled : RoutePattern := parsePattern $pat
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
