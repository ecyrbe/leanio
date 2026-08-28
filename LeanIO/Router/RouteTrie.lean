module

public import Std.Http
public import Std.Async
public import LeanIO.Router.RoutePattern
public import LeanIO.Router.Route

namespace LeanIO.Router
open Std Http Server
open Std.Async

deriving instance Hashable for Method
deriving instance BEq for Method
deriving instance ReflBEq for Method
deriving instance LawfulBEq for Method

/--
A segment-based trie for O(depth) route dispatch instead of O(routes) linear scan.

Each node holds handlers at this path depth, literal edges (exact segment match),
a param edge (`{param}`) for single-segment path parameters, and a wildcard edge
(`{*rest}`) for lowest-priority remainder capture.

Priority: literal > param (`{param}`) > wildcard (`{*rest}`)

Uses `HashMap` for handlers and `HashMap.Raw` for literals to allow recursive definition
-/
public structure RouteTrie where
  handlers : HashMap Method HandlerFn     := ∅
  literals : HashMap.Raw String RouteTrie := ∅
  param    : Option (String × RouteTrie)  := none
  wildcard : Option (String × RouteTrie)  := none
  /-- Fallback for a request matching no route. Only the root node's is consulted. -/
  notFound : Option HandlerFn             := none

namespace RouteTrie

/-- Empty trie with no routes. -/
public def empty : RouteTrie := {}

public instance : Inhabited RouteTrie := ⟨empty⟩

public instance : EmptyCollection RouteTrie := ⟨empty⟩

/--
Inserts a route into the trie given its method, segment list, and composed handler.

Middlewares should be composed onto the handler before insertion.
-/
public def addRoute (self : RouteTrie) (method : Method) (segs : List Segment) (handler : HandlerFn) : RouteTrie :=
  match segs with
  | [] => { self with handlers := self.handlers.insert method handler }
  | Segment.lit s :: rest =>
    let child := self.literals.get? s |>.getD empty
    { self with literals := self.literals.insert s (addRoute child method rest handler) }
  | Segment.param name :: rest =>
    let child := match self.param with | some (_, c) => c | none => empty
    { self with param := some (name, addRoute child method rest handler) }
  | Segment.rest name :: rest =>
    let child := match self.wildcard with | some (_, c) => c | none => empty
    { self with wildcard := some (name, addRoute child method rest handler) }

/--
Adds a route from a runtime pattern string (e.g. `"/user/{id}"`).
Useful for programmatic (non-macro) route construction.
-/
public def addRouteFromPattern (self : RouteTrie) (method : Method) (pattern : String) (handler : HandlerFn) : RouteTrie :=
  let pat := RoutePattern.ofString pattern
  addRoute self method pat.segments handler

/--
Builds a trie from a list of `Route` values, composing route-level middlewares.
-/
public def ofRoutes (routes : List Route) : RouteTrie :=
  routes.foldl (init := empty) fun self r =>
    let h := r.middlewares.foldl (fun f mw => mw f) r.handler
    self.addRoute r.method r.pat.segments h

/--
The outcome of a lookup. `methodMismatch` carries the methods registered at the
matched path, which is what an `Allow` header needs; it is distinct from
`noMatch` so that a wrong method can be answered with 405 rather than 404.
-/
public inductive Lookup where
  | found (params : List (String × String)) (handler : HandlerFn)
  | methodMismatch (allowed : List Method)
  | noMatch

/--
Combines two lookups in priority order, so `literal <|> param <|> wildcard`
keeps its meaning: any `found` wins, and a path that matched under some other
method is only reported once every branch has failed to produce a handler.
-/
public def Lookup.orElse : Lookup → (Unit → Lookup) → Lookup
  | .found params h, _ => .found params h
  | .noMatch, k => k ()
  | .methodMismatch a, k =>
    match k () with
    | .found params h => .found params h
    | .methodMismatch b => .methodMismatch (a ++ b).eraseDups
    | .noMatch => .methodMismatch a

/-- Whether the lookup produced a handler. -/
public def Lookup.isFound : Lookup → Bool
  | .found .. => true
  | _ => false

/-- The handler and its captures, discarding why a lookup failed. -/
public def Lookup.toOption : Lookup → Option (List (String × String) × HandlerFn)
  | .found params handler => some (params, handler)
  | _ => none

/-- The handlers stored at a node, as a `found` or a `methodMismatch`/`noMatch`. -/
def handlerAt (t : RouteTrie) (method : Method) (params : List (String × String)) : Lookup :=
  match t.handlers.get? method with
  | some h => .found params h
  | none =>
    match t.handlers.keys with
    | [] => .noMatch
    | ms => .methodMismatch ms

/--
Looks up a handler by method and path segments. Priority: literal > param > wildcard.

Reports `found` with the captured parameters, `methodMismatch` with the methods
the path does accept, or `noMatch`.
-/
public def lookup (self : RouteTrie) (method : Method) (segs : List String) : Lookup :=
  go self segs []
where
  go (t : RouteTrie) (segs : List String) (params : List (String × String)) : Lookup :=
    match segs with
    -- leaf, do we have a handler ?
    | [] => handlerAt t method params
    | seg :: rest =>
    -- literal match ?
      (match t.literals.get? seg with
       | some child => go child rest params
       | none => .noMatch)
      |>.orElse (fun _ =>
      -- or else param match ? An empty segment is not a parameter value: a path
      -- ending in `/` decodes to a trailing `""`.
        if seg.isEmpty then .noMatch else
        match t.param with
        | some (name, child) => go child rest (params ++ [(name, seg)])
        | none => .noMatch)
      |>.orElse (fun _ =>
      -- or else wildcard match ?
        match t.wildcard with
        | some (name, child) =>
          handlerAt child method (params ++ [(name, String.intercalate "/" (seg :: rest))])
        | none => .noMatch)

/--
Walks the trie depth-first, calling `f method segs handler acc` for every stored
handler, where `segs` is the reconstructed path of `Segment` values leading to it
with the original parameter names preserved.

Example: given a trie containing

  ```
  GET /todos             → h1
  POST /todos            → h2
  GET /todos/{id}        → h3
  ```

`fold trie (fun m segs h acc => (m, segs) :: acc) []` produces

  ```lean4
  [(GET,  [lit "todos", param "id"]),
   (POST, [lit "todos"]),
   (GET,  [lit "todos"])]
  ```

(order is deterministic but insertion-dependent, not route-priority).
-/
public partial def fold (self : RouteTrie) (f : Method → List Segment → HandlerFn → α → α) (init : α) : α :=
  foldGo f self [] init
where
  foldGo (f : Method → List Segment → HandlerFn → α → α) (t : RouteTrie) (revSegs : List Segment) (acc : α) : α :=
    let acc := HashMap.fold (fun acc m h => f m revSegs.reverse h acc) acc t.handlers
    let acc := HashMap.Raw.fold (fun acc s child => foldGo f child (Segment.lit s :: revSegs) acc) acc t.literals
    let acc := match t.param with | none => acc | some (name, child) => foldGo f child (Segment.param name :: revSegs) acc
    let acc := match t.wildcard with | none => acc | some (name, child) => foldGo f child (Segment.rest name :: revSegs) acc
    acc

/-- `Allow` header value for a set of methods, with `HEAD` implied by `GET`. -/
public def allowValue (methods : List Method) : String :=
  let methods := if methods.contains .get && !(methods.contains .head)
                 then methods ++ [Method.head] else methods
  String.intercalate "," (methods.map toString)

/--
Dispatches an incoming request through the trie: a single O(depth) lookup.

On match, captured path parameters are injected into the request's extensions
as `RouteParams` and the stored handler is invoked. Middlewares are expected to
be pre-composed onto handlers before insertion, nothing is composed here.

A path that exists under other methods answers 405 with `Allow`, except for
`HEAD`, which is served by the `GET` handler with the body dropped. Anything
unmatched goes to `notFound`, whatever the method.

The `HEAD` response carries `Content-Length: 0` rather than the length the body
would have had: `Std.Http`'s writer has no `HEAD` awareness, so declaring a
length it will not send would strand the client waiting for it.
-/
public def dispatch (self : RouteTrie) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  let method := req.line.method
  let path := req.line.uri.path
  let segments := path.toDecodedSegments.toList
  match self.lookup method segments with
  | .found params handler => run params handler
  | .methodMismatch allowed =>
    if method == .head && allowed.contains .get then
      match self.lookup .get segments with
      | .found params handler =>
        let response ← run params handler
        return { response with body := Body.Any.ofBody ({} : Body.Empty) }
      | _ => methodNotAllowed allowed
    else
      methodNotAllowed allowed
  | .noMatch =>
    match self.notFound with
    | some handler => handler req
    | none => Response.notFound |>.text s!"Not Found: {method} {path}"
where
  run (params : List (String × String)) (handler : HandlerFn) : ContextAsync (Response Body.Any) :=
    handler { req with extensions := req.extensions.insert { params : RouteParams } }
  methodNotAllowed (allowed : List Method) : ContextAsync (Response Body.Any) :=
    let builder := Response.new.status .methodNotAllowed
    (builder.header? "allow" (allowValue allowed) |>.getD builder) |>.empty

/-- Makes `RouteTrie` usable as a `Std.Http.Server.Handler`. -/
public instance : Handler RouteTrie where
  onRequest := dispatch

end LeanIO.Router.RouteTrie
