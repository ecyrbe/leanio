module

public import LeanIn
public import LeanIn.Http.H1
public import LeanIn.Http.Server
public import LeanIO.Router.RouteTrie
public import Std.Http.Data.Method
public import Lean
public import LeanIO.Data.Headers.MimeType
public import LeanIO.Response.BrowserCached
public import LeanIO.Response.File.RangeFile
public import LeanIO.Request.FromRequestBody
public import LeanIO.Request.FromRequestParts
public import LeanIO.Request.HeaderRange
public import LeanIO.Response.File.File
public import LeanIO.Response.File.Utils
public import LeanIO.Data.Headers.MimeType
public import LeanIO.Response.BrowserCached
public import LeanIO.Response.File.RangeFile
public import LeanIO.Request.MultiPartForm.Api
public import LeanIO.Request.Form

public section

/-!
# The LeanIn backend

Routes stored over a LeanIn handler value (`RouteTrie Handler`) and dispatched on a LeanIn connection. A
handler sees a `Request`: the parsed head, the streaming body reader, and the path parameters the trie captured
when the route has parameters. Middleware is a handler transformer, composed before insertion, as in the
upstream trie.

The application-facing effect signature and the reasoning obligations are
`docs/notes/LeanIO_to_LeanIn_Effect_Signature.md`. The streaming body reader and response writer are
implemented. A buffered extractor accumulates the whole body through `readAll`, and on this streaming path that
accumulation is **not bounded by `H1.Limits.maxBody`**: `readAll` keeps appending as bytes arrive, so it may grow
with the input. The `maxBody` bound belongs to the separate buffered driver, which rejects an oversize
`Content-Length` body with 413 before dispatch. A request's own transport failure is the **peer-I/O request
scope**: a failed read, a stall, a peer close mid-body or a failed response send is a typed VM abort that the
streaming driver's scope turns into a closed connection, never a catchable error (`docs/decisions.md` D23).
Still later stages of `docs/notes/LeanIO_to_LeanIn_Porting_Handoff.md`: child/task cancellation, shutdown/drain
and scope-exit cleanup.
-/

namespace LeanIO.Backend.LeanIn

open LeanIn
open LeanIn.Http
open LeanIO.Router
open Std.Http

/-- The driver's handler: the streaming session shape. -/
abbrev CoreHandler := LeanIn.Http.Server.StreamHandler

/-- A request's cancellation/cleanup scope. Children share it; the scope owner sets `cancelled` on exit and
runs `cleanups` exactly once, after the handler's outcome is fixed. -/
structure Scope where
  cancelled : IO.Ref Bool
  cleanups : IO.Ref (Array (LeanIn.Runtime.VM Unit))

/-- Lift a `BaseIO` action (reference operations and the like) into the VM. -/
def liftBase {α : Type} (act : BaseIO α) : LeanIn.Runtime.VM α :=
  monadLift (BaseIO.toIO act)

/-- A request as the native backend sees it. `body` is the streaming reader; an extractor that needs the
whole body reads it, and a route that streams consumes it incrementally. `params` are the trie's captures for
a parameterised route, in the order the pattern names them. -/
structure Request where
  head : H1.Head
  body : LeanIn.Http.Server.BodyReader
  params : List (String × String) := []
  /-- Request extensions, as in LeanIO's `Request`: middleware can insert typed values and a handler can read
  them back with `Extensions.get`. -/
  extensions : Std.Http.Extensions := .empty
  /-- The request scope, when dispatched through the backend. -/
  scope : Option Scope := none
  /-- The peer's address, when the connection's shim can supply it. -/
  peer : Option Std.Net.SocketAddress := none

/-- A native handler: the request and the response writer, so a response need not be accumulated. -/
abbrev Handler := Request → LeanIn.Http.Server.ResponseWriter → LeanIn.Runtime.VM Unit

/-- Read a whole body through the reader. This is what a buffered extractor (`jsonBody`, `formBody`) does; a
streaming route calls `recv` itself and never accumulates. -/
partial def readAll (r : LeanIn.Http.Server.BodyReader) (acc : ByteArray) : LeanIn.Runtime.VM ByteArray := do
  match ← r.recv 65536 with
  | some chunk => readAll r (acc ++ chunk)
  | none => return acc

/-- Middleware transforms a handler; composition order is nesting order (the leftmost/outermost runs first). -/
abbrev Middleware := Handler → Handler

/-- Applies middlewares the way the upstream router does: a left fold, so the **last** one added runs first
(outermost). -/
def applyMiddlewares (ms : Array Middleware) : Middleware :=
  ms.foldl (fun h mw => mw h)

/-- One declared route: method, pattern, handler and its own middlewares (innermost). -/
structure NativeRoute where
  method : Std.Http.Method
  pattern : RoutePattern
  handler : Handler
  middlewares : Array Middleware := #[]

/-- A router with the upstream composition semantics: route middlewares innermost, then the router's, mounts
prefixed, and a `notFound` for misses. `toTrie` compiles it once, as upstream does at startup. -/
structure NativeRouter where
  routes : Array NativeRoute := #[]
  mounts : Array (String × NativeRouter) := #[]
  middlewares : Array Middleware := #[]
  notFound : Option Handler := none

namespace NativeRouter

def empty : NativeRouter := {}

/-- Add a route value, as built by `nativeRoute`. -/
def add (self : NativeRouter) (r : NativeRoute) : NativeRouter :=
  { self with routes := self.routes.push r }

/-- Add a route from a pattern string such as `/user/{id}`. -/
def route (self : NativeRouter) (method : Std.Http.Method) (pattern : String) (handler : Handler) :
    NativeRouter :=
  let r : NativeRoute := { method, pattern := RoutePattern.ofString pattern, handler }
  { self with routes := self.routes.push r }

/-- Add a route with its own middlewares; they run inside the router's. -/
def routeWith (self : NativeRouter) (method : Std.Http.Method) (pattern : String) (handler : Handler)
    (middlewares : Array Middleware) : NativeRouter :=
  let r : NativeRoute := { method, pattern := RoutePattern.ofString pattern, handler, middlewares }
  { self with routes := self.routes.push r }

/-- Append a router middleware. With the fold in `applyMiddlewares`, the last added runs first. -/
def addMiddleware (mw : Middleware) (self : NativeRouter) : NativeRouter :=
  { self with middlewares := self.middlewares.push mw }

/-- Mount a sub-router under a prefix. -/
def mount (self : NativeRouter) (pre : String) (sub : NativeRouter) : NativeRouter :=
  { self with mounts := self.mounts.push (pre, sub) }

/-- The handler for requests matching no route. -/
def withNotFound (handler : Handler) (self : NativeRouter) : NativeRouter :=
  { self with notFound := some handler }

/-- Compile the router into a trie, composing middlewares once. -/
partial def toTrie (self : NativeRouter) : RouteTrie Handler :=
  let wrap := applyMiddlewares self.middlewares
  let trie := self.routes.foldl (fun acc r =>
    acc.addRoute r.method r.pattern.segments (wrap (applyMiddlewares r.middlewares r.handler)))
    (RouteTrie.empty : RouteTrie Handler)
  let trie := self.mounts.foldl (fun acc (pre, sub) =>
    let preSegs := (RoutePattern.ofString pre).segments
    RouteTrie.fold sub.toTrie (fun method segs handler acc =>
      acc.addRoute method (preSegs ++ segs) (wrap handler)) acc) trie
  { trie with notFound := self.notFound.map wrap }

end NativeRouter

/-- The method token as `Std.Http.Method`, or `none` when the token is not one it names. Unknown methods are
refused explicitly at dispatch; they are never mapped onto a different method. -/
def method? (h : H1.Head) : Option Std.Http.Method :=
  Std.Http.Method.ofString? h.methodTokenString

/-- Percent-decode `s`; `+` becomes a space when `plusIsSpace`, and stays literal in a path. -/
def pctDecode (s : String) (plusIsSpace : Bool) : String := Id.run do
  let bs := s.toUTF8
  let hex (c : UInt8) : Option Nat :=
    if c ≥ 0x30 && c ≤ 0x39 then some (c - 0x30).toNat
    else if c ≥ 0x61 && c ≤ 0x66 then some (c - 0x61 + 10).toNat
    else if c ≥ 0x41 && c ≤ 0x46 then some (c - 0x41 + 10).toNat
    else none
  let mut out := ByteArray.empty
  let mut i := 0
  while i < bs.size do
    let c := bs.get! i
    if c == 0x25 && i + 2 < bs.size then
      match hex (bs.get! (i + 1)), hex (bs.get! (i + 2)) with
      | some hi, some lo => out := out.push (hi * 16 + lo).toUInt8; i := i + 3
      | _, _ => out := out.push c; i := i + 1
    else if plusIsSpace && c == 0x2B then
      out := out.push 0x20; i := i + 1
    else
      out := out.push c; i := i + 1
  return (String.fromUTF8? out).getD s

/-- The request target without its query. -/
def Request.path (r : Request) : String :=
  let t := r.head.targetString
  (t.splitOn "?").headD t

/-- The path segments, query removed and each percent-decoded. -/
def pathSegments (h : H1.Head) : List String :=
  let t := h.targetString
  let path := (t.splitOn "?").headD t
  (path.splitOn "/").filter (fun s => !s.isEmpty) |>.map (fun s => pctDecode s false)

/-- The raw query string (after `?` and before `#`), if any. -/
def Request.queryString? (r : Request) : Option String :=
  match r.head.targetString.splitOn "?" with
  | _ :: q :: _ => some ((q.splitOn "#").headD q)
  | _ => none

/-- The query parameters, decoded by LeanIO's query parser. Only `none` when the query is malformed. -/
def Request.query? (r : Request) : Option Std.Http.URI.Query :=
  match r.queryString? with
  | none => some {}
  | some q =>
    match LeanIO.parseQuery {} q.toUTF8.iter with
    | .success _ query => some query
    | .error _ _ => none

/-- A query parameter by name, decoded; `none` when absent, undecodable, or the query is malformed. -/
def Request.queryParam? (r : Request) (name : String) : Option String :=
  match r.query? with
  | none => none
  | some q =>
    let pairs : Array (Std.Http.URI.EncodedQueryParam × Option Std.Http.URI.EncodedQueryParam) := q
    let found : Option (Std.Http.URI.EncodedQueryParam × Option Std.Http.URI.EncodedQueryParam) :=
      pairs.find? (fun p => p.1.decode == some name)
    found.bind (fun p => p.2.bind (fun v => v.decode))

/-- A captured path parameter by name. -/
def Request.pathParam? (r : Request) (name : String) : Option String :=
  r.params.find? (·.1 == name) |>.map (·.2)

/-- The request method as `Std.Http.Method`, or `none` for a token Std does not name. -/
def Request.stdMethod? (r : Request) : Option Std.Http.Method := method? r.head

/-- The request's headers as `Std.Http.Headers`, preserving names and repeated values. -/
def Request.stdHeaders (r : Request) : Std.Http.Headers :=
  r.head.headers.foldl (fun acc (n, v) =>
    let name := (H1.Slice.toString? r.head.buf n).getD ""
    let value := (H1.Slice.toString? r.head.buf v).getD ""
    acc.insert! name value) Std.Http.Headers.empty

/-- The request target as `Std.Http.RequestTarget`; origin-form is what the H1 parser accepts. -/
def Request.stdTarget (r : Request) : Std.Http.RequestTarget :=
  let target := r.head.targetString
  let pathPart := (target.splitOn "?").headD target
  let segments := (pathPart.splitOn "/").filter (fun s => !s.isEmpty)
    |>.map Std.Http.URI.EncodedSegment.encode |>.toArray
  let path : Std.Http.URI.Path := { segments, absolute := pathPart.startsWith "/" }
  let query := if r.queryString?.isSome then r.query? else none
  .originForm path query

/-- A stock-shaped request line: `req.line.method`, `.version`, `.uri`, `.headers`. This is the view the
LeanIO middleware and handlers read; the native request also carries the streaming body and typed extensions.
An explicit `(req : Request Body.Stream)` annotation remains a stock type and is not this view. -/
def Request.line (r : Request) : Std.Http.Request.Head :=
  { method := (r.stdMethod?).getD .get
    version := (match r.head.version with
      | H1.Version.v10 => .v10
      | H1.Version.v11 => .v11)
    uri := r.stdTarget
    headers := r.stdHeaders }

/-- The first header value with this name, as text. -/
def Request.header? (r : Request) (name : String) : Option String :=
  H1.Head.headerValue? r.head name.toUTF8

/-- The media type of `Content-Type`, lowercased and without parameters. -/
def Request.mediaType? (r : Request) : Option String :=
  (r.header? "content-type").map fun v =>
    (v.splitOn ";" |>.headD v).trimAscii.toString.toLower

/-- Whether the request's media type matches one the extractor type names. A request with no `Content-Type`
does not match a type declaring media types: accepting it silently would make a JSON route answer to
form-encoded or absent bodies. -/
def Request.mimeOk (r : Request) (t : Type) [LeanIO.MimeType.HasMimeTypes t] : Bool :=
  match LeanIO.MimeType.HasMimeTypes.mimes? t with
  | none => true
  | some mimes =>
    match r.mediaType? with
    | none => false
    | some actual => mimes.any fun m => m.value.toLower == actual

/-- Decode a JSON body. The failures are distinguishable: 415 for the wrong media type, 400 for malformed
JSON, 422 when it parses but does not fit `α`. -/
def jsonBody (α : Type) [Lean.FromJson α] [LeanIO.MimeType.HasMimeTypes (LeanIO.Json α)] (r : Request) :
    LeanIn.Runtime.VM (Except (Nat × String) α) := do
  if !r.mimeOk (LeanIO.Json α) then
    return .error (415, "unsupported media type")
  else
    let b ← readAll r.body .empty
    return (match String.fromUTF8? b with
    | none => .error (400, "body is not valid UTF-8")
    | some text =>
      match Lean.Json.parse text with
      | .error e => .error (400, s!"malformed JSON: {e}")
      | .ok j =>
        match Lean.FromJson.fromJson? (α := α) j with
        | .ok a => .ok a
        | .error e => .error (422, s!"body does not fit: {e}"))

/-- A plain-text body, with the same media-type check. -/
def textBody (r : Request) : LeanIn.Runtime.VM (Except (Nat × String) String) := do
  if !r.mimeOk LeanIO.PlainText then
    return .error (415, "unsupported media type")
  else
    let b ← readAll r.body .empty
    return (match String.fromUTF8? b with
    | none => .error (400, "body is not valid UTF-8")
    | some text => .ok text)

/-- Decode an `application/x-www-form-urlencoded` body. The query parser and `FromForm` are LeanIO's; only the
transport (buffered bytes instead of a stock body stream) is the backend's. -/
def formBody (α : Type) [LeanIO.FromForm α] [LeanIO.MimeType.HasMimeTypes (LeanIO.Form α)] (r : Request) :
    LeanIn.Runtime.VM (Except (Nat × String) α) := do
  if !r.mimeOk (LeanIO.Form α) then
    return .error (415, "unsupported media type")
  else
    let b ← readAll r.body .empty
    return (match LeanIO.parseQuery {} b.iter with
    | .success _ query =>
      match LeanIO.FromForm.fromForm (α := α) query with
      | .ok v => .ok v
      | .error e => .error (422, s!"body does not fit: {e}")
    | .error _ _ => .error (400, "malformed form body"))

/-- The reason phrase the backend uses for its own statuses. -/
def reasonFor : Nat → String
  | 400 => "Bad Request"
  | 401 => "Unauthorized"
  | 403 => "Forbidden"
  | 404 => "Not Found"
  | 405 => "Method Not Allowed"
  | 413 => "Payload Too Large"
  | 415 => "Unsupported Media Type"
  | 422 => "Unprocessable Entity"
  | 431 => "Request Header Fields Too Large"
  | 501 => "Not Implemented"
  | _   => "Internal Server Error"

/-- Strip one pair of surrounding double quotes. -/
def unquote (s : String) : String :=
  if s.length ≥ 2 && s.startsWith "\"" && s.endsWith "\"" then
    (s.drop 1).dropEnd 1 |>.toString
  else s

/-- The `boundary` parameter of a `multipart/form-data` content type. -/
def boundary? (r : Request) : Option String :=
  (r.header? "content-type").bind fun v =>
    (v.splitOn ";").findSome? fun p =>
      match p.trimAscii.toString.splitOn "=" with
      | ["boundary", b] => some (unquote (b.trimAscii.toString))
      | _ => none

/-- The native body source for the upstream multipart parser. -/
instance : LeanIO.BodySource LeanIn.Runtime.VM LeanIn.Http.Server.BodyReader where
  recv r := r.recv 65536

/-- Build the upstream `MultiPartForm` over the native request reader. -/
def multipartForm (req : Request) :
    LeanIn.Runtime.VM (Except (Nat × String)
      (LeanIO.MultiPartForm LeanIn.Runtime.VM LeanIn.Http.Server.BodyReader)) := do
  if req.mediaType? != some "multipart/form-data" then
    return .error (415, "unsupported media type")
  match boundary? req with
  | none => return .error (400, "missing multipart boundary")
  | some b =>
    let boundSep := ("\r\n--" ++ b).toUTF8
    let inner : LeanIO.MultipartInner LeanIn.Runtime.VM LeanIn.Http.Server.BodyReader := {
      boundStart := boundSep.extract 2 boundSep.size
      boundSepSearch := LeanIO.Search.new (LeanIO.ChunkBuffer.ofByteArray boundSep)
      stream := req.body }
    let ref ← (monadLift (IO.mkRef inner) : LeanIn.Runtime.VM _)
    return .ok { inner := ref }

/-- Compose the upstream multipart API with a handler. The entries and `FormFile` operations are the same
types the stock backend uses, instantiated at `VM`/`BodyReader`. -/
def withMultipartForm
    (f : LeanIO.MultiPartForm LeanIn.Runtime.VM LeanIn.Http.Server.BodyReader → Handler) :
    Handler :=
  fun req w => do
    match ← multipartForm req with
    | .ok mp => f mp req w
    | .error (status, msg) =>
      w.start { status, reason := reasonFor status,
                headers := #[("Content-Length", toString msg.toUTF8.size)] }
      w.write msg.toUTF8
      w.finish

/-- A response with one buffered body and a `Content-Length`. -/
def response (status : Nat) (reason : String) (body : ByteArray)
    (headers : Array (String × String) := #[]) : H1.ResponseHead × ByteArray :=
  ({ status, reason, headers := headers.push ("Content-Length", toString body.size) }, body)

/-- A buffered response through the streaming writer: `Content-Length` and one write. -/
def respond (w : LeanIn.Http.Server.ResponseWriter) (status : Nat) (reason : String)
    (body : ByteArray) (headers : Array (String × String) := #[]) : LeanIn.Runtime.VM Unit := do
  w.start { status, reason, headers := headers.push ("Content-Length", toString body.size) }
  w.write body
  w.finish

/-- The application environment a route body sees under the parameterised syntax: the request and the response
writer, so a body can read and write without receiving them as arguments. -/
structure Env where
  req : Request
  writer : LeanIn.Http.Server.ResponseWriter

/-- The native application monad. A body written in the route syntax runs in `AppM`; the handler supplies the
environment once. -/
abbrev AppM := ReaderT Env LeanIn.Runtime.VM

namespace AppM

/-- The request being handled. -/
def request : AppM Request := fun env => pure env.req

/-- Send a buffered response: head with `Content-Length`, one write, finish. -/
def respond (status : Nat) (reason : String) (body : ByteArray)
    (headers : Array (String × String) := #[]) : AppM Unit :=
  fun env => LeanIO.Backend.LeanIn.respond env.writer status reason body headers

/-- The response writer, for a body that streams. -/
def writer : AppM LeanIn.Http.Server.ResponseWriter := fun env => pure env.writer

/-- The request body reader, for a route that streams the request. -/
def bodyReader : AppM LeanIn.Http.Server.BodyReader := fun env => pure env.req.body

/-- Abort if the request scope has been cancelled. Cooperative, like `ContextAsync.checkCancelled`: a
computation that never checks continues until it finishes. -/
def checkCancelled : AppM Unit := fun env =>
  match env.req.scope with
  | none => pure ()
  | some scope => do
    if (← liftBase scope.cancelled.get) then
      .abortE (IO.userError "request cancelled")
    else pure ()

/-- Register a cleanup to run once when the request exits, after children have been asked to stop. -/
def onExit (cleanup : AppM Unit) : AppM Unit := fun env => do
  match env.req.scope with
  | none => pure ()
  | some scope => liftBase (scope.cleanups.modify (·.push (cleanup env)))

/-- Start a request-owned child on the same carrier. The child shares the scope's cancellation flag; it stops
when it next calls `checkCancelled`. -/
def spawn (child : AppM Unit) : AppM Unit := fun env => do
  monadLift (LeanIn.Runtime.VM.runAsync (child env) (fun _ => pure ()) : IO Unit)

end AppM

/-- A request-body extraction in the native application monad. The failure is the status and message to answer
with, as in the buffered extractors. -/
class FromRequestBodyM (α : Type) where
  read : Env → LeanIn.Runtime.VM (Except (Nat × String) α)

/-- JSON bodies in the native monad. -/
instance [Lean.FromJson α] [LeanIO.MimeType.HasMimeTypes (LeanIO.Json α)] : FromRequestBodyM (LeanIO.Json α) where
  read env := do
    match ← jsonBody α env.req with
    | .ok a => pure (.ok { body := a })
    | .error e => pure (.error e)

/-- Plain-text bodies in the native monad. -/
instance : FromRequestBodyM LeanIO.PlainText where
  read env := do
    match ← textBody env.req with
    | .ok text => pure (.ok { body := text })
    | .error e => pure (.error e)

/-- A form-encoded body in the native monad. -/
instance [LeanIO.FromForm α] [LeanIO.MimeType.HasMimeTypes (LeanIO.Form α)] :
    FromRequestBodyM (LeanIO.Form α) where
  read env := do
    match ← LeanIO.Backend.LeanIn.formBody α env.req with
    | .ok a => pure (.ok { value := a })
    | .error e => pure (.error e)

/-- A body accepted as either of two types, chosen by media type, mirroring LeanIO's `α ⊕ β` instance. -/
instance [FromRequestBodyM α] [LeanIO.MimeType.HasMimeTypes α]
    [FromRequestBodyM β] [LeanIO.MimeType.HasMimeTypes β] : FromRequestBodyM (α ⊕ β) where
  read env := do
    if env.req.mimeOk α then
      match ← FromRequestBodyM.read (α := α) env with
      | .ok a => pure (.ok (.inl a))
      | .error e => pure (.error e)
    else if env.req.mimeOk β then
      match ← FromRequestBodyM.read (α := β) env with
      | .ok b => pure (.ok (.inr b))
      | .error e => pure (.error e)
    else pure (.error (415, "unsupported media type"))

/-- The raw body bytes in the native monad. -/
instance : FromRequestBodyM ByteArray where
  read env := do
    return .ok (← readAll env.req.body .empty)

/-- A request-parts extraction in the native backend. As in the stock classes, a failure is the status and
message to answer with. -/
class FromRequestPartsM (α : Type) where
  readParts : Request → Except (Nat × String) α

/-- `Path α` from a named path-parameter map (`FromPath`). -/
instance [LeanIO.FromPath α] : FromRequestPartsM (LeanIO.Path α) where
  readParts r :=
    match LeanIO.FromPath.fromPath (Std.HashMap.ofList r.params) with
    | .ok v => .ok { value := v }
    | .error e => .error (400, e)

/-- `Path α` from the single captured parameter (`FromString`). -/
instance [LeanIO.FromString α] : FromRequestPartsM (LeanIO.Path α) where
  readParts r :=
    if r.params.length != 1 then
      .error (400, "expected exactly one path parameter")
    else
      match LeanIO.FromString.parse r.params[0]!.2 with
      | .ok v => .ok { value := v }
      | .error e => .error (400, e)

/-- The request's query (`Std.Http.URI.Query`), or 400 when it is malformed. -/
instance : FromRequestPartsM Std.Http.URI.Query where
  readParts r :=
    match r.query? with
    | some q => .ok q
    | none => .error (400, "malformed query")

/-- A response value in the native backend, mirroring `LeanIO.IntoResponse`: the value a route body returns
rather than writing the writer by hand. -/
class IntoResponseM (α : Type) where
  write : Env → α → AppM Unit

/-- Run a route body and write whatever response value it returned. -/
def runResponse {α : Type} [IntoResponseM α] (m : AppM α) : AppM Unit := fun env => do
  (IntoResponseM.write env (← m env)) env

instance : IntoResponseM Unit where
  write _ _ := pure ()

instance : IntoResponseM String where
  write _ text :=
    AppM.respond 200 "OK" text.toUTF8 #[("Content-Type", "text/plain; charset=utf-8")]

instance : IntoResponseM ByteArray where
  write _ bytes :=
    AppM.respond 200 "OK" bytes #[("Content-Type", "application/octet-stream")]

instance : IntoResponseM IO.Error where
  write _ e := AppM.respond 500 "Internal Server Error" (toString e).toUTF8

instance : IntoResponseM Std.Http.Status where
  write _ status :=
    AppM.respond status.toCode.toNat (status.reasonPhrase) .empty

instance : IntoResponseM (Std.Http.Status × String) where
  write _ p :=
    let (status, text) := p
    AppM.respond status.toCode.toNat status.reasonPhrase text.toUTF8
      #[("Content-Type", "text/plain; charset=utf-8")]

instance [Lean.ToJson α] : IntoResponseM (Std.Http.Status × α) where
  write _ p :=
    let (status, value) := p
    AppM.respond status.toCode.toNat status.reasonPhrase
      (Lean.toJson value).compress.toUTF8 #[("Content-Type", "application/json")]

instance [IntoResponseM ε] [IntoResponseM α] : IntoResponseM (Except ε α) where
  write env
    | .ok a => IntoResponseM.write env a
    | .error e => IntoResponseM.write env e

instance : IntoResponseM (H1.ResponseHead × ByteArray) where
  write env p := do
    let (head, body) := p
    env.writer.start head
    env.writer.write body
    env.writer.finish

/-- Stream a file handle to the response writer. -/
partial def streamFileHandle (h : IO.FS.Handle)
    (w : LeanIn.Http.Server.ResponseWriter) : LeanIn.Runtime.VM Unit := do
  let bytes ← (monadLift (h.read (USize.ofNat 65536) : IO ByteArray))
  if bytes.isEmpty then w.finish
  else
    w.write bytes
    streamFileHandle h w

/-- A file on disk, streamed with `Content-Length` and an ETag; an `If-None-Match` hit answers 304. -/
instance : IntoResponseM LeanIO.File where
  write env f := do
    let p := f.path
    let fileExists ← (monadLift (p.pathExists : IO Bool))
    let directory ← (monadLift (p.isDir : IO Bool))
    if !fileExists || directory then
      (AppM.respond 404 "Not Found" .empty) env
    else
      let md ← (monadLift (p.metadata : IO IO.FS.Metadata))
      let size := md.byteSize.toNat
      let etag := LeanIO.computeETag md
      if env.req.header? "if-none-match" == some (toString etag) then
        (AppM.respond 304 "Not Modified" .empty #[("ETag", toString etag)]) env
      else do
        let handle ← (monadLift (IO.FS.Handle.mk p .read : IO IO.FS.Handle))
        let cacheHeaders := match f.cacheControl with
          | some cc => #[("Cache-Control", toString cc)]
          | none => #[]
        let head : H1.ResponseHead :=
          { status := 200
            reason := "OK"
            headers := cacheHeaders ++ #[("Content-Length", toString size),
                         ("Content-Type", toString (LeanIO.MimeType.mimeType p)),
                         ("ETag", toString etag)] }
        env.writer.start head
        streamFileHandle handle env.writer

/-- Select the first range, marking an unsatisfiable start as `(fileSize, 0)` so the caller answers 416. (The
stock `RangeFile.pickRange` maps that case to `(0,0)`, which would make a 0-length 206.) -/
def pickRange (ranges : Option (Array LeanIO.Range)) (fileSize : Nat) : Option (Nat × Nat) :=
  match ranges with
  | none => none
  | some rs =>
    match rs[0]? with
    | none => none
    | some r =>
      match r.start, r.stop with
      | some s, some e => if s >= fileSize then some (fileSize, 0) else some (s, min e (fileSize - 1) - s + 1)
      | some s, none => if s >= fileSize then some (fileSize, 0) else some (s, fileSize - s)
      | none, some suffix =>
        let suffix := min suffix fileSize
        some (fileSize - suffix, suffix)
      | none, none => some (0, fileSize)

/-- Stream at most `n` bytes from a file handle. -/
partial def streamFileHandleN (h : IO.FS.Handle) (w : LeanIn.Http.Server.ResponseWriter)
    (n : Nat) : LeanIn.Runtime.VM Unit := do
  if n == 0 then w.finish
  else
    let want := USize.ofNat (min n 65536)
    let bytes ← (monadLift (h.read want : IO ByteArray))
    if bytes.isEmpty then w.finish
    else
      w.write bytes
      streamFileHandleN h w (n - bytes.size)

/-- A file with `Range` support: 206 + `Content-Range` for a satisfiable range, 416 when the start is past
the end, and ETag/304 before either. -/
instance : IntoResponseM LeanIO.RangeFile where
  write env f := do
    let p := f.path
    let fileExists ← (monadLift (p.pathExists : IO Bool))
    let directory ← (monadLift (p.isDir : IO Bool))
    if !fileExists || directory then
      (AppM.respond 404 "Not Found" .empty) env
    else
      let md ← (monadLift (p.metadata : IO IO.FS.Metadata))
      let size := md.byteSize.toNat
      let etag := LeanIO.computeETag md
      if env.req.header? "if-none-match" == some (toString etag) then
        (AppM.respond 304 "Not Modified" .empty #[("ETag", toString etag)]) env
      else
        let handle ← (monadLift (IO.FS.Handle.mk p .read : IO IO.FS.Handle))
        let ranges := (env.req.header? "range").bind LeanIO.parseRange
        let ctype := toString (LeanIO.MimeType.mimeType p)
        let cacheHeaders := match f.cacheControl with
          | some cc => #[("Cache-Control", toString cc)]
          | none => #[]
        match pickRange ranges size with
        | none =>
          let head : H1.ResponseHead :=
            { status := 200, reason := "OK",
              headers := cacheHeaders ++ #[("Content-Length", toString size), ("Content-Type", ctype),
                           ("ETag", toString etag), ("Accept-Ranges", "bytes")] }
          env.writer.start head
          streamFileHandleN handle env.writer size
        | some (start, len) =>
          if start >= size then
            let head : H1.ResponseHead :=
              { status := 416, reason := "Range Not Satisfiable",
                headers := cacheHeaders ++ #[("Content-Range", s!"bytes */{size}"), ("Content-Length", "0"),
                             ("ETag", toString etag)] }
            env.writer.start head
            env.writer.finish
          else
            let last := start + len - 1
            monadLift (LeanIO.skipBytes handle start : IO Unit)
            let head : H1.ResponseHead :=
              { status := 206, reason := "Partial Content",
                headers := cacheHeaders ++ #[("Content-Length", toString len), ("Content-Type", ctype),
                             ("Content-Range", s!"bytes {start}-{last}/{size}"),
                             ("ETag", toString etag), ("Accept-Ranges", "bytes")] }
            env.writer.start head
            streamFileHandleN handle env.writer len

/-- A JSON response with an ETag and cache-control, answering 304 on `If-None-Match`. -/
instance [Lean.ToJson α] : IntoResponseM (LeanIO.BrowserCached α) where
  write env cached := do
    let json := (Lean.toJson cached.value).compress
    let etag := Std.Http.Header.Value.ofString! s!"\"{json.hash}\""
    if env.req.header? "if-none-match" == some (toString etag) then
      (AppM.respond 304 "Not Modified" .empty #[("ETag", toString etag)]) env
    else
      (AppM.respond 200 "OK" json.toUTF8
        #[("Content-Type", "application/json"), ("ETag", toString etag),
          ("Cache-Control", toString cached.cacheControl)]) env

/-- A route body in the native backend: either a response value or an `AppM` computation. This is what lets
the unprefixed route syntax keep LeanIO's `=> "text"` and `=> do …` forms. -/
class RunBody (F : Type) where
  runBody : F → AppM Unit

instance [IntoResponseM R] : RunBody R where
  runBody r := fun env => (IntoResponseM.write env r) env

instance [IntoResponseM R] : RunBody (AppM R) where
  runBody m := runResponse m

/-- Apply a route body's response conversion. -/
def runBody {F : Type} [RunBody F] (f : F) : AppM Unit := RunBody.runBody f

/-- The request method as `Std.Http.Method`; an unknown token is refused rather than collapsed. -/
instance : FromRequestPartsM Std.Http.Method where
  readParts r :=
    match method? r.head with
    | some m => .ok m
    | none => .error (501, "method not supported")

/-- The HTTP version as `Std.Http.Version`. -/
instance : FromRequestPartsM Std.Http.Version where
  readParts r :=
    .ok (match r.head.version with
      | H1.Version.v10 => .v10
      | H1.Version.v11 => .v11)

/-- The request headers as `Std.Http.Headers`; duplicate names are preserved by `insert!`. -/
instance : FromRequestPartsM Std.Http.Headers where
  readParts r :=
    let hs := r.head.headers.foldl (fun acc (n, v) =>
      let name := (H1.Slice.toString? r.head.buf n).getD ""
      let value := (H1.Slice.toString? r.head.buf v).getD ""
      acc.insert! name value) Std.Http.Headers.empty
    .ok hs

/-- A structure whose fields are query parameters, parsed by `FromQuery` (so `deriving FromQuery` works). -/
instance [LeanIO.FromQuery α] : FromRequestPartsM (LeanIO.Query α) where
  readParts r :=
    match r.query? with
    | none => .error (400, "malformed query")
    | some q =>
      match LeanIO.FromQuery.fromQuery (α := α) q with
      | .ok v => .ok { value := v }
      | .error e => .error (400, e)

/-- The request target as Std's request-target type. -/
instance : FromRequestPartsM Std.Http.RequestTarget where
  readParts r := .ok r.stdTarget

/-- The URI path of the request target. -/
instance : FromRequestPartsM Std.Http.URI.Path where
  readParts r := .ok r.stdTarget.path

/-- The parsed `Range` header, as LeanIO's `HeaderRange`. -/
instance : FromRequestPartsM LeanIO.HeaderRange where
  readParts r :=
    match r.header? "range" with
    | some range => .ok { ranges := LeanIO.parseRange range }
    | none => .ok { ranges := none }

/-- The client's remote address, when the shim could read it. -/
instance : FromRequestPartsM Std.Http.Server.RemoteAddr where
  readParts r :=
    match r.peer with
    | some addr => .ok { addr }
    | none => .error (500, "remote address not available")

/-- The binders after a body extractor: a body that is already in `AppM`. -/
class NativePartsExtractor (Fn : Type) where
  extractParts : Fn → Handler

/-- Adapt a route body's binders to a native handler, mirroring `LeanIO.Extractor` for the stock backend. -/
class NativeExtractor (Fn : Type) where
  extract : Fn → Handler

instance : NativePartsExtractor (AppM Unit) where
  extractParts f req w := f { req, writer := w }

instance [FromRequestBodyM P] [NativePartsExtractor Rest] : NativeExtractor (P → Rest) where
  extract f req w := do
    match ← FromRequestBodyM.read (α := P) { req, writer := w } with
    | .ok p => NativePartsExtractor.extractParts (f p) req w
    | .error (status, msg) => LeanIO.Backend.LeanIn.respond w status (reasonFor status) msg.toUTF8

instance [FromRequestPartsM P] [NativePartsExtractor Rest] : NativeExtractor (P → Rest) where
  extract f req w := do
    match FromRequestPartsM.readParts (α := P) req with
    | .ok p => NativePartsExtractor.extractParts (f p) req w
    | .error (status, msg) => LeanIO.Backend.LeanIn.respond w status (reasonFor status) msg.toUTF8

instance [FromRequestPartsM P] [NativePartsExtractor Rest] : NativePartsExtractor (P → Rest) where
  extractParts f req w := do
    match FromRequestPartsM.readParts (α := P) req with
    | .ok p => NativePartsExtractor.extractParts (f p) req w
    | .error (status, msg) => LeanIO.Backend.LeanIn.respond w status (reasonFor status) msg.toUTF8

instance : NativeExtractor (Unit → AppM Unit) where
  extract f req w := f () { req, writer := w }

instance : NativeExtractor (AppM Unit) where
  extract f req w := f { req, writer := w }

/-- Error scope as a handler: `VM.attempt` carries the catch through every suspension, so a handler that
parks and then raises is caught here exactly as one that raises at once. The failure becomes a 500 carrying the
error's `toString`, rather than escaping the connection.

A transport failure never arrives here: inside the streaming request scope the body reader and the response
writer classify it on the VM's separate abort channel, which `attempt` does not catch, so no further response is
attempted for a cancelled request instead of a 500. The driver's own raw operations outside that scope are not VM
effects at all: a failed send or read prompts a close attempt and reports the transport error, while a refused
close is reported as its own error and does not establish that the connection closed. Only an
ordinary raise — including one whose message happens to begin `Http:` — is this middleware's. -/
def catchErrors : Middleware :=
  fun handler req w => do
    match ← LeanIn.Runtime.VM.attempt (handler req w) with
    | .ok _ => pure ()
    | .error e =>
      monadLift (IO.eprintln s!"leanio: 500: {e}")
      respond w 500 "Internal Server Error" (toString e).toUTF8

/-- Observing middleware: logs method, path and status through `IO` lifted into the machine, then returns the
response unchanged. -/
def requestLogger (label : String) : Middleware :=
  fun handler req w => do
    handler req w
    let code ← w.code
    let line := label ++ " " ++ req.head.methodTokenString ++ " " ++ req.path ++ " -> " ++ toString (code.getD 0)
    monadLift (do IO.println line; IO.getStdout >>= fun s => s.flush)

/-- Compose an extractor with a handler: an extraction failure becomes the response, so applications compose
`withExtract (jsonBody T) fun t => …` the way the stock extractor classes compose. -/
def withExtract (extract : Request → LeanIn.Runtime.VM (Except (Nat × String) α)) (f : α → Handler) :
    Handler :=
  fun req w => do
    match ← extract req with
    | .ok a => f a req w
    | .error (status, msg) => respond w status (reasonFor status) msg.toUTF8

/-- A writer that runs the handler but drops the body, for HEAD from a GET route. -/
def discardBody (w : LeanIn.Http.Server.ResponseWriter) :
    LeanIn.Http.Server.ResponseWriter := {
  sendContinue := w.sendContinue
  interim := w.interim
  start := fun rh =>
    let hs := (rh.headers.filter (fun (n, _) => n.toLower != "content-length")).push ("Content-Length", "0")
    w.start { rh with headers := hs }
  write := fun _ => pure ()
  finish := w.finish
  status := w.status
  code := w.code }

/-- Run one handler with a fresh request scope: children are asked to stop, cleanups run exactly once, and
the handler's outcome is re-raised (an abort stays an abort). -/
def runScoped (handler : Handler) (req : Request) (w : LeanIn.Http.Server.ResponseWriter) : LeanIn.Runtime.VM Unit := do
  let cancelled ← (monadLift (IO.mkRef (false : Bool) : IO (IO.Ref Bool)) : LeanIn.Runtime.VM _)
  let cleanups ← (monadLift (IO.mkRef (#[] : Array (LeanIn.Runtime.VM Unit)) :
    IO (IO.Ref (Array (LeanIn.Runtime.VM Unit)))) : LeanIn.Runtime.VM _)
  let req := { req with scope := some { cancelled, cleanups } }
  let outcome ← LeanIn.Runtime.VM.requestScope (handler req w)
  liftBase (cancelled.set true)
  let cs ← liftBase cleanups.get
  for c in cs.reverse do
    c
  match outcome with
  | .ok _ => pure ()
  | .error e => throw e
  | .aborted reason => .abortE reason

/-- Dispatch a parsed head and streaming body through the trie.

An unknown method token is refused with 501 rather than dispatched. A path known under other methods answers 405
with `Allow`; a miss goes to the trie's `notFound` handler or a 404. The trie's captured parameters become the
request's `params`. -/
def dispatch (trie : RouteTrie Handler) : CoreHandler := fun head body w =>
  match method? head with
  | none => respond w 501 "Not Implemented" "method not supported\n".toUTF8
  | some m =>
    match trie.lookup m (pathSegments head) with
    | .found params handler => runScoped handler { head, body, params, peer := body.peer } w
    | .methodMismatch allowed =>
      -- LeanIO serves HEAD from the GET handler and drops the body; the response declares
      -- `Content-Length: 0` rather than the length the body would have had, because declaring a length it
      -- will not send would strand the client. This matches the upstream trie's documented behaviour.
      if m == .head && allowed.contains .get then
        match trie.lookup .get (pathSegments head) with
        | .found params handler => runScoped handler { head, body, params, peer := body.peer } (discardBody w)
        | _ => respond w 405 "Method Not Allowed" "".toUTF8 #[("Allow", RouteTrie.allowValue allowed)]
      else
        respond w 405 "Method Not Allowed" "".toUTF8 #[("Allow", RouteTrie.allowValue allowed)]
    | .noMatch =>
      match trie.notFound with
      | some handler => runScoped handler { head, body, peer := body.peer } w
      | none => respond w 404 "Not Found" "not found\n".toUTF8

/-- Serve `handler` on `port` with the given hooks (so an application can build handlers against the same live
clock, e.g. to suspend on a timer). Blocks until the accept loop ends. -/
def serveWith (hooks : LeanIn.Runtime.Hooks) (handler : CoreHandler) (port : UInt16) : IO UInt32 := do
  let l ← LeanIn.Runtime.Listener.bind (LeanIn.Runtime.loopback port)
  let e ← LeanIn.Sched.Executor.new LeanIn.Task.Item 256 1
  let cfg : LeanIn.Http.Server.Config := { clock := LeanIn.Runtime.Clock.live hooks }
  let prog : LeanIn.Task.EAsync IO.Error Unit :=
    LeanIn.Runtime.serveForever hooks l (LeanIn.Http.Server.serveConnStream cfg · handler)
  let outcome : Except IO.Error Unit ← LeanIn.Runtime.run e prog
  LeanIn.Runtime.Io.stop
  match outcome with
  | .ok _ => return 0
  | .error err => IO.eprintln s!"the accept loop ended: {err}"; return 1

/-- A running native server: `stop` stops admission and lets in-flight connections drain; `join` returns the
accept loop's outcome. -/
structure ServerHandle where
  executor : LeanIn.Sched.Executor LeanIn.Task.Item 256
  task : _root_.Task (Except IO.Error (Except IO.Error Unit))

namespace ServerHandle

/-- Stop accepting; `serveUntilStopped` drains the connections already accepted. -/
def stop (h : ServerHandle) : IO Unit := h.executor.stop

/-- Wait for the accept loop and its drain to finish. -/
def join (h : ServerHandle) : IO (Except IO.Error Unit) := do
  match ← IO.wait h.task with
  | .ok outcome => pure outcome
  | .error e => pure (.error e)

end ServerHandle

/-- Start serving without blocking: the accept loop runs on a dedicated thread, and the returned handle stops
and joins it. Uses `serveUntilStopped`, so `stop` is a drain rather than an abort. -/
def serveWithHandleH1 (hooks : LeanIn.Runtime.Hooks) (h1 : LeanIn.Http.H1.Limits)
    (handler : CoreHandler) (port : UInt16) : IO ServerHandle := do
  let l ← LeanIn.Runtime.Listener.bind (LeanIn.Runtime.loopback port)
  let e ← LeanIn.Sched.Executor.new LeanIn.Task.Item 256 1
  let cfg : LeanIn.Http.Server.Config := { h1, clock := LeanIn.Runtime.Clock.live hooks }
  let prog : LeanIn.Task.EAsync IO.Error Unit :=
    LeanIn.Runtime.serveUntilStopped hooks e l 20
      (LeanIn.Http.Server.serveConnStream cfg · handler) []
  let task ← IO.asTask (LeanIn.Runtime.run e prog) .dedicated
  return { executor := e, task }

/-- `serveWithHandle` with the protocol's limits chosen by the caller. The default drops keep-alive after 100
messages; a benchmark or a long-lived service can raise that. -/
def serveWithHandle (hooks : LeanIn.Runtime.Hooks) (handler : CoreHandler) (port : UInt16) :
    IO ServerHandle :=
  serveWithHandleH1 hooks {} handler port

/-- Serve `handler` on `port`, creating the hooks. -/
def serve (handler : CoreHandler) (port : UInt16) : IO UInt32 := do
  let hooks ← LeanIn.Runtime.Hooks.new
  serveWith hooks handler port

end LeanIO.Backend.LeanIn
end
