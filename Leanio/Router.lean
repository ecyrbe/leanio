import Lean
import Std.Http
import Leanio.RouteParam
import Leanio.Utils
open Std Http Server
open Std.Async
open Leanio.Utils

namespace Leanio.Router

abbrev HandlerSig := Request Body.Stream → ContextAsync (Response Body.Any)

def splitPath (path : String) : List String :=
  path.split '/' |>.filter (¬ ·.isEmpty) |>.map toString |>.toList

/--
  Route pattern:
  - Sum.inl : literal string segment
  - Sum.inr : route variable name
-/
structure RoutePattern where
  segments : List (Sum String String)

structure Route where
  method     : Method
  pat        : RoutePattern
  handler    : HandlerSig
  middlewares : List (HandlerSig → HandlerSig) := []

def parsePattern (path : String) : RoutePattern :=
  { segments := splitPath path |>.map fun s =>
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
  matchImpl pattern.segments (splitPath path)

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

def Router.empty : Router :=
  { routes := [], routers := [], middlewares := [] }

def Router.addRoute (route : Route) (r : Router) : Router :=
  { r with routes := r.routes ++ [route] }

def Router.addRouter (pre : String) (sub : Router) (r : Router) : Router :=
  { r with routers := r.routers ++ [(pre, sub)] }

def Router.addMiddleware (mw : HandlerSig → HandlerSig) (r : Router) : Router :=
  { r with middlewares := r.middlewares ++ [mw] }

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

-- ==========================================
-- State injection via Extensions
-- ==========================================

/--
Creates a middleware that injects `state` into every request's extensions.
Use with a wrapper struct that derives `TypeName`:

```lean4
structure MyState where
  ref : IO.Ref AppState
deriving TypeName

let ref ← IO.mkRef initialState
Router.addMiddleware (withState MyState { ref := ref }) router
```
-/
def withState (α : Type) [TypeName α] (state : α) (next : HandlerSig) : HandlerSig := fun req =>
  next { req with extensions := req.extensions.insert state }

/--
Extracts a value of type `α` from request extensions.
Returns `none` if no middleware injected the value.
-/
def getState (α : Type) [TypeName α] (req : Request Body.Stream) : Option α :=
  req.extensions.get α

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

  -- validate pattern structure and param names
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

      if paramBinders.isEmpty then
        `(fun ($reqId : $reqTy) => $body)
      else
        if paramBinders.length ≠ n then
          Macro.throwErrorAt pat s!"handler has {paramBinders.length} parameter(s) but pattern has {n} path parameter(s)"

        -- validate that binder names match path parameter names
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
