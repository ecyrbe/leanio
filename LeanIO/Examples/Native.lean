import LeanIO.Request.DeriveFromQuery
import LeanIO.Router.Macro
import LeanIO.Backend.LeanIn
import LeanIO.Backend.LeanIn.Middlewares

/-!
# `leanioNative` — the first native LeanIn-backed LeanIO server

Two routes over the generic route trie, dispatched by `LeanIO.Backend.LeanIn` on a LeanIn connection:

- `GET /ping` answers immediately;
- `GET /slow` parks in `VM.sleep` for 200 ms before answering, so a sibling request can complete while it is
  suspended.

This is the port's first milestone: routing and dispatch are LeanIO's (the trie is the upstream one, parameterised
over its handler value), the connection, handler execution and clock are LeanIn's, and no stock `Async`,
`ContextAsync` or `Std.Http.Server` is on the serving path.
-/

open LeanIn
open LeanIO.Router
open LeanIO.Backend

abbrev Handler := LeanIO.Backend.LeanIn.Handler
abbrev Middleware := LeanIO.Backend.LeanIn.Middleware

namespace LeanIO.Examples.Native

open LeanIn.Http

def ping : Handler :=
  fun _ w => LeanIO.Backend.LeanIn.respond w 200 "OK" "pong\n".toUTF8

def slow (clock : LeanIn.Runtime.Clock) : Handler :=
  fun _ w => do
    let _ ← LeanIn.Runtime.VM.sleep clock 200
    LeanIO.Backend.LeanIn.respond w 200 "OK" "slept\n".toUTF8

/-- A JSON body type: the first extraction shown over the native request pair. -/
structure Todo where
  title : String
  done : Bool := false
deriving Lean.FromJson, Lean.ToJson, Repr

def reasonFor : Nat → String
  | 413 => "Payload Too Large"
  | 415 => "Unsupported Media Type"
  | 422 => "Unprocessable Entity"
  | _   => "Bad Request"

/-- JSON extraction: 415 for the wrong media type, 400 for malformed JSON, 422 when it does not fit. -/
def postTodo : Handler :=
  LeanIO.Backend.LeanIn.withExtract (LeanIO.Backend.LeanIn.jsonBody Todo) fun todo _req w =>
    LeanIO.Backend.LeanIn.respond w 200 "OK" (Lean.toJson todo).compress.toUTF8

/-- Plain-text extraction with the same media-type contract. -/
def postText : Handler :=
  LeanIO.Backend.LeanIn.withExtract LeanIO.Backend.LeanIn.textBody fun text _req w =>
    LeanIO.Backend.LeanIn.respond w 200 "OK" s!"text: {text}\n".toUTF8

/-- Path parameters captured by the trie (`/user/{id}`), plus a query parameter. -/
def userById : Handler :=
  fun req w =>
    let id := (req.pathParam? "id").getD ""
    let name := (req.queryParam? "name").getD ""
    LeanIO.Backend.LeanIn.respond w 200 "OK" s!"id={id} name={name}\n".toUTF8

/-- Raises *after* a suspension, to exercise an error scope across a park. -/
def boom (clock : LeanIn.Runtime.Clock) : Handler :=
  fun _ _ => do
    let _ ← LeanIn.Runtime.VM.sleep clock 100
    throw (IO.userError "boom after a park")

/-- Middleware demonstration: adds a header to every response it wraps. -/
def tag (value : String) : LeanIO.Backend.LeanIn.Middleware :=
  fun handler req w =>
    let w' : LeanIn.Http.Server.ResponseWriter := {
      sendContinue := w.sendContinue
      interim := w.interim
      start := fun rh => w.start { rh with headers := rh.headers.push ("X-Tag", value) }
      write := w.write
      finish := w.finish
      status := w.status
      code := w.code }
    handler req w'

/-- Form extraction: LeanIO's query parser and `FromForm` over the buffered body. -/
def postForm : Handler :=
  fun req w => do
    match (← (LeanIO.Backend.LeanIn.formBody (Std.HashMap String String) req
            : LeanIn.Runtime.VM (Except (Nat × String) (Std.HashMap String String)))) with
    | .ok m =>
      let user := (m.get? "user").getD ""
      LeanIO.Backend.LeanIn.respond w 200 "OK" s!"user={user}\n".toUTF8
    | .error (status, msg) => LeanIO.Backend.LeanIn.respond w status (reasonFor status) msg.toUTF8

/-- The route declaration syntax with the native backend selected explicitly: a body in `AppM` and no
handler-valued argument. -/
def nping : LeanIO.Backend.LeanIn.NativeRoute :=
  native GET "/nping" => do
    LeanIO.Backend.LeanIn.AppM.respond 200 "OK" "native-syntax\n".toUTF8

/-- The same syntax with a body extractor: the binder is read through the native `FromRequestBodyM` instance. -/
def ntodo : LeanIO.Backend.LeanIn.NativeRoute :=
  native POST "/ntodos" (t : LeanIO.Json Todo) => do
    LeanIO.Backend.LeanIn.AppM.respond 200 "OK" (Lean.toJson t.body).compress.toUTF8

/-- Path-parameter extraction in the native route syntax. -/
def nuser : LeanIO.Backend.LeanIn.NativeRoute :=
  native GET "/nuser/{id}" (id : LeanIO.Path Nat) => do
    LeanIO.Backend.LeanIn.AppM.respond 200 "OK" s!"id={id.value}\n".toUTF8

/-- Form extraction in the native route syntax. -/
def nform : LeanIO.Backend.LeanIn.NativeRoute :=
  native POST "/nform" (f : LeanIO.Form (Std.HashMap String String)) => do
    LeanIO.Backend.LeanIn.AppM.respond 200 "OK" s!"user={(f.value.get? "user").getD ""}\n".toUTF8

section
set_option leanio.backend.native true

/-- Stock-style response values: plain string, status tuples, extractor binders and JSON. -/
def dropinText : LeanIO.Backend.LeanIn.NativeRoute :=
  GET "/dropin" => "ok\n"

def dropinStatus : LeanIO.Backend.LeanIn.NativeRoute :=
  GET "/dropin-status" => (Std.Http.Status.created, "made\n")

def dropinPath : LeanIO.Backend.LeanIn.NativeRoute :=
  GET "/dropin/{id}" (id : LeanIO.Path Nat) => (Std.Http.Status.ok, s!"id={id.value}\n")

def dropinJson : LeanIO.Backend.LeanIn.NativeRoute :=
  POST "/dropin-json" (t : LeanIO.Json Todo) => (Std.Http.Status.ok, t.body)

/-- The same route syntax with no backend prefix: `set_option leanio.backend.native true` selects the native
backend for this scope. -/
def npingOption : LeanIO.Backend.LeanIn.NativeRoute :=
  GET "/nping-option" => do
    LeanIO.Backend.LeanIn.AppM.respond 200 "OK" "option-native\n".toUTF8

end

/-- Count a file body by streaming it, without keeping the bytes. -/
def countStream (f : LeanIO.FormFile LeanIn.Runtime.VM LeanIn.Http.Server.BodyReader) :
    LeanIn.Runtime.VM Nat := do
  let r ← (monadLift (IO.mkRef (0 : Nat)) : LeanIn.Runtime.VM _)
  f.stream fun chunk => monadLift (r.modify (· + chunk.size) : IO Unit)
  monadLift (r.get : IO Nat)

/-- Extension middleware demo: the value inserted by `withExtension`. -/
structure Meta where
  value : String
deriving TypeName

def extensionDemo : Handler :=
  fun req w => do
    match req.extensions.get Meta with
    | some m => LeanIO.Backend.LeanIn.respond w 200 "OK" s!"meta={m.value}\n".toUTF8
    | none => LeanIO.Backend.LeanIn.respond w 500 "Internal Server Error" "missing extension".toUTF8

/-- Basic auth with the native middleware; validator runs in `VM`. -/
def authDemo : LeanIO.Backend.LeanIn.Middlewares.AuthConfig :=
  .basic fun user pass => pure (user == "admin" && pass.value == "secret")

def secureDemo : Handler :=
  fun _ w => LeanIO.Backend.LeanIn.respond w 200 "OK" "secret-area\n".toUTF8

/-- Parts extractors beyond Path/Query: method and version. -/
def methodDemo : LeanIO.Backend.LeanIn.NativeRoute :=
  native GET "/method" (m : Std.Http.Method) (v : Std.Http.Version) =>
    (Std.Http.Status.ok, s!"{m} {v}\n")

/-- Either JSON or plain text, chosen by media type. -/
def sumBodyDemo : LeanIO.Backend.LeanIn.NativeRoute :=
  native POST "/sum" (b : LeanIO.Json Todo ⊕ LeanIO.PlainText) =>
    match b with
    | .inl j => (Std.Http.Status.ok, s!"json:{j.body.title}\n")
    | .inr t => (Std.Http.Status.ok, s!"text:{t.body}\n")

initialize cleanupCount : IO.Ref Nat ← IO.mkRef 0
initialize childCount : IO.Ref Nat ← IO.mkRef 0

/-- A route that registers an exit cleanup and responds immediately. -/
def cleanupDemo : Handler :=
  fun req w => do
    let env : LeanIO.Backend.LeanIn.Env := { req, writer := w }
    LeanIO.Backend.LeanIn.AppM.onExit
      (fun _ => LeanIO.Backend.LeanIn.liftBase (cleanupCount.modify (· + 1))) env
    LeanIO.Backend.LeanIn.respond w 200 "OK" "scheduled\n".toUTF8

/-- A route that starts a request-owned child and responds without waiting. -/
def childDemo : Handler :=
  fun req w => do
    let env : LeanIO.Backend.LeanIn.Env := { req, writer := w }
    LeanIO.Backend.LeanIn.AppM.spawn
      (fun _ => LeanIO.Backend.LeanIn.liftBase (childCount.modify (· + 1))) env
    LeanIO.Backend.LeanIn.respond w 200 "OK" "spawned\n".toUTF8

/-- The cleanup/child counters, for the fixture. -/
def countersDemo : Handler :=
  fun _ w => do
    let c ← LeanIO.Backend.LeanIn.liftBase cleanupCount.get
    let k ← LeanIO.Backend.LeanIn.liftBase childCount.get
    LeanIO.Backend.LeanIn.respond w 200 "OK" s!"cleanup={c} child={k}\n".toUTF8

/-- The Range header extractor. -/
def rangeDemo : LeanIO.Backend.LeanIn.NativeRoute :=
  native GET "/range" (hr : LeanIO.HeaderRange) =>
    (Std.Http.Status.ok, s!"ranges={hr.ranges.isSome}\n")

/-- A file response, streamed with an ETag. -/
def fileDemo : LeanIO.Backend.LeanIn.NativeRoute :=
  native GET "/file" => ({ path := "/tmp/leanio-native-file.txt" : LeanIO.File })

/-- A browser-cached JSON response. -/
def cachedDemo : LeanIO.Backend.LeanIn.NativeRoute :=
  native GET "/cached" => LeanIO.BrowserCached.new ({ title := "cached" : Todo })

/-- A range file response. -/
def rangeFileDemo : LeanIO.Backend.LeanIn.NativeRoute :=
  native GET "/range-file" => ({ path := "/tmp/leanio-native-file.txt" : LeanIO.RangeFile })

/-- A derived query structure and a struct whose fields derive from path/query. -/
structure PageParams where
  page : Nat := 1
  size : Nat := 10
deriving FromQuery

def queryDemo : LeanIO.Backend.LeanIn.NativeRoute :=
  native GET "/query" (q : LeanIO.Query PageParams) =>
    (Std.Http.Status.ok, s!"page={q.value.page} size={q.value.size}\n")

/-- The client's remote address. -/
def remoteDemo : LeanIO.Backend.LeanIn.NativeRoute :=
  native GET "/remote" (ra : Std.Http.Server.RemoteAddr) =>
    (Std.Http.Status.ok, s!"{ra.addr}\n")

/-- A body transform: uppercase each chunk through the native stateful map. -/
def upperDemo : Handler :=
  fun req w => do
    let r ← (monadLift (req.body.mapState
      (fun _ chunk => pure ((), (String.fromUTF8? chunk).getD "" |>.toUpper.toUTF8)) ()
      : LeanIn.Runtime.VM _))
    let body ← r.readAll
    LeanIO.Backend.LeanIn.respond w 200 "OK" body

/-- The stock-shaped request line: middleware and handlers may read `req.line.*`. -/
def lineDemo : Handler :=
  fun req w =>
    let host := ((req.line.headers.get? .host).map (·.value)).getD ""
    LeanIO.Backend.LeanIn.respond w 200 "OK"
      s!"{req.line.method} {req.line.uri} host={host}\n".toUTF8

/-- The upstream multipart API on the native reader: each entry is a field or a `FormFile`. -/
partial def uploadWalk (mp : LeanIO.MultiPartForm LeanIn.Runtime.VM LeanIn.Http.Server.BodyReader)
    (i : Nat) (acc : String) : LeanIn.Http.Server.ResponseWriter → LeanIn.Runtime.VM Unit
  | w => do
    match ← mp.nextEntry with
    | none => LeanIO.Backend.LeanIn.respond w 200 "OK" s!"parts={i} {acc.trimAscii.toString}\n".toUTF8
    | some (.field name value) =>
      uploadWalk mp (i + 1) (acc ++ s!"{name}:-:{value.toUTF8.size}:text/plain ") w
    | some (.file f) =>
      let n ← countStream f
      uploadWalk mp (i + 1) (acc ++ s!"{f.name}:{f.filename}:{n}:{f.contentType} ") w

def upload : Handler :=
  LeanIO.Backend.LeanIn.withMultipartForm fun mp _req w => uploadWalk mp 0 "" w

/-- Count streamed parts with `FormFile.stream`. -/
partial def uploadStreamWalk
    (mp : LeanIO.MultiPartForm LeanIn.Runtime.VM LeanIn.Http.Server.BodyReader)
    (i : Nat) (acc : String) : LeanIn.Http.Server.ResponseWriter → LeanIn.Runtime.VM Unit
  | w => do
    match ← mp.nextEntry with
    | none => LeanIO.Backend.LeanIn.respond w 200 "OK" s!"streamed={i} {acc}\n".toUTF8
    | some (.field name value) =>
      uploadStreamWalk mp (i + 1) (acc ++ s!"{name}:{value.toUTF8.size};") w
    | some (.file f) =>
      let n ← countStream f
      uploadStreamWalk mp (i + 1) (acc ++ s!"{f.name}:{n};") w

def uploadStream : Handler :=
  LeanIO.Backend.LeanIn.withMultipartForm fun mp _req w => uploadStreamWalk mp 0 "" w

/-- Save each file part to `/tmp/leanio-part-<n>`; fields are consumed as values. -/
partial def saveParts (mp : LeanIO.MultiPartForm LeanIn.Runtime.VM LeanIn.Http.Server.BodyReader)
    (i : Nat) : LeanIn.Http.Server.ResponseWriter → LeanIn.Runtime.VM Unit
  | w => do
    match ← mp.nextEntry with
    | none => LeanIO.Backend.LeanIn.respond w 200 "OK" s!"saved={i}\n".toUTF8
    | some (.field _ value) =>
      monadLift (IO.FS.writeBinFile s!"/tmp/leanio-part-{i}" value.toUTF8 : IO Unit)
      saveParts mp (i + 1) w
    | some (.file f) =>
      f.save s!"/tmp/leanio-part-{i}"
      saveParts mp (i + 1) w

def uploadSave : Handler :=
  LeanIO.Backend.LeanIn.withMultipartForm fun mp _req w => saveParts mp 0 w

/-- Echo the request body. The reader delivers it incrementally; this route accumulates it to answer with one
`Content-Length` body, while a streaming route would `recv` and `write` instead. -/
def echo : Handler :=
  fun req w => do
    let body ← req.body.readAll
    LeanIO.Backend.LeanIn.respond w 200 "OK" body

def routes (clock : LeanIn.Runtime.Clock) : LeanIO.Backend.LeanIn.NativeRouter :=
  LeanIO.Backend.LeanIn.NativeRouter.empty
    |>.add (nativeRoute GET "/ping" ping)
    |>.route .get "/bench" (fun _ w =>
      LeanIO.Backend.LeanIn.respond w 200 "OK"
        "hello from std\nmethod=GET path=/bench version=HTTP/1.1\n".toUTF8)
    |>.route .get "/slow" (slow clock)
    |>.route .get "/user/{id}" (tag "users" userById)
    |>.route .post "/echo" echo
    |>.route .post "/todos" postTodo
    |>.route .post "/text" postText
    |>.route .post "/form" postForm
    |>.route .post "/upload" upload
    |>.route .post "/upload-stream" uploadStream
    |>.route .post "/upload-save" uploadSave
    |>.add nping
    |>.add nuser
    |>.add nform
    |>.add npingOption
    |>.add dropinText
    |>.add dropinStatus
    |>.add dropinPath
    |>.add dropinJson
    |>.add methodDemo
    |>.route .get "/line" lineDemo
    |>.route .post "/upper" upperDemo
    |>.add fileDemo
    |>.add rangeFileDemo
    |>.add cachedDemo
    |>.add queryDemo
    |>.add remoteDemo
    |>.add rangeDemo
    |>.route .get "/cleanup" cleanupDemo
    |>.route .get "/child" childDemo
    |>.route .get "/counters" countersDemo
    |>.add sumBodyDemo
    |>.route .get "/ext" extensionDemo
    |>.routeWith .get "/auth" secureDemo #[LeanIO.Backend.LeanIn.Middlewares.auth authDemo]
    |>.addMiddleware (LeanIO.Backend.LeanIn.Middlewares.withExtension Meta { value := "hello" })
    |>.add ntodo
    |>.route .get "/boom" (boom clock)
    |>.route .get "/logged" (LeanIO.Backend.LeanIn.requestLogger "req" ping)
    |>.addMiddleware LeanIO.Backend.LeanIn.catchErrors

end LeanIO.Examples.Native

open LeanIO.Examples.Native

def main (args : List String) : IO UInt32 := do
  let port : UInt16 := ((args.headD "8787").toNat?.getD 8787).toUInt16
  let hooks ← LeanIn.Runtime.Hooks.new
  let clock := LeanIn.Runtime.Clock.live hooks
  let h ← LeanIO.Backend.LeanIn.serveWithHandleH1 hooks { maxMessages := 1000000000 }
    (LeanIO.Backend.LeanIn.dispatch (routes clock).toTrie) port
  match ← h.join with
  | .ok _ => return 0
  | .error e => IO.eprintln s!"the accept loop ended: {e}"; return 1
