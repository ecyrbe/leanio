# LeanIO: a fork maintained in this repository

Base: `ecyrbe/leanio` at `0bb212d57f3dba2d76cf601f76a7c431a51c902e`, MIT licensed (see `LICENSE`; the upstream
copyright notice is retained). This copy is a **fork**, not a vendored snapshot: it is modified here, and its
backend is being moved onto LeanIn. Upstream changes are not merged automatically.

## Divergence log

| date | change | why |
| --- | --- | --- |
| 2026-10-20 | `Data/Headers/HeaderName.lean`: removed `contentEncoding`, `ifNoneMatch`, `lastModified`, `ifModifiedSince` | `Std.Http.Header.Name` provides them in Lean 4.35.0 |
| 2026-10-20 | tests moved from `Tests.*` to `LeanIO.Tests.*` | they live inside the fork's namespace |
| 2026-10-20 | `Router.RouteTrie` parameterised over its handler value (`RouteTrie H`); `RouteTrie HandlerFn` is the upstream instantiation | the LeanIn backend needs the same trie, lookup and fold over a native handler type; `dispatch` and the `Std.Http.Server.Handler` instance stay typed at `HandlerFn` |
| 2026-10-20 | `Router.Macro`: route syntax is an elaborator; `leanio.backend.native` option; `native … => …` and `nativeRoute` syntax; `Backend.LeanIn` handlers take `BodyReader`/`ResponseWriter`; `AppM` and native extractor classes | the unprefixed route syntax selects a backend by option, and streaming request and response bodies |
| 2026-10-20 | `Backend.LeanIn`: request-aware `IntoResponseM`; native `File` (streamed, ETag/304), `RangeFile` (206/Content-Range, 416) and `BrowserCached` (JSON, ETag/304) | file, range and cached responses work natively; RemoteAddr/derive/BodyExt remain |
| 2026-10-20 | `Backend.LeanIn`: request `Scope` with `onExit`, cooperative `checkCancelled`, and `spawn`, dispatched around every handler | request-owned children and exactly-once cleanup exist natively; cancellation is cooperative and shutdown/drain remains outstanding |
| 2026-10-20 | `Server.BodyReader.readAll` | the stock `body.readAll` call shape exists on the native reader; it accumulates, so bounded bodies only |
| 2026-10-20 | `Backend.LeanIn`: Std `Method`/`Version`/`Headers` parts extractors and a `Request.line` view | stock-style middleware/handler accessors work without rewriting; only an explicit stock request type remains a source change |
| 2026-10-20 | `Backend.LeanIn.Middlewares`: `catchErrors`, `requestLogger`, `withExtension`, `auth` over the native handler; native `Request` carries typed extensions | the built-in middleware is usable natively with the stock names/parameters; only custom HandlerFn middleware remains a source change |
| 2026-10-20 | `Backend.LeanIn`: `IntoResponseM`/`RunBody`, so stock-style route bodies (`=> "text"`, status tuples, JSON values, `Except`, `ResponseHead × ByteArray`) work on the native backend; native `FromRequestBodyM` for `Form`/`ByteArray`; `FromRequestPartsM` for `Path` and `URI.Query` | the native route syntax can use the body and parts binder forms, not only JSON/text |
| 2026-10-20 | `MultiPartForm` parameterised over `BodySource m body` (defaults `ContextAsync`/`Body.Stream`); native `BodySource VM BodyReader`; `readUntilGo` extracts from the read's start position | one upstream multipart parser serves both backends; the cursor fix makes headers split across reads parse correctly |

## Port status

The fork's backend has been moved onto LeanIn for the native path:

| piece | state |
| --- | --- |
| routing | upstream trie parameterised over its handler value; `NativeRouter` provides the upstream composition semantics natively — route middlewares innermost, then the router's with the last added outermost, mounts prefixing, `notFound`, and a one-time `toTrie` — and `LeanIO.Backend.LeanIn` dispatches it on a LeanIn connection |
| method | token preserved by `H1.Head`, mapped to `Std.Http.Method` or refused (501); a known method with no route is 405 with `Allow`; HEAD is served by the GET handler with the body dropped and `Content-Length: 0`, as upstream documents |
| bodies | native handlers receive a streaming `BodyReader`; a buffered extractor's `readAll` accumulates the whole body and is **not** capped by `H1.Limits.maxBody` on this path, so it may grow with the input — the cap belongs to the separate buffered driver, which answers an oversize `Content-Length` body 413 before dispatch; malformed/oversized heads answer 400/431 before close |
| extraction | JSON, plain text and URL-encoded form over the buffered body (415/400/422), reusing LeanIO's MIME declarations, query parser and `FromForm` |
| request | path parameters captured by the trie, query string/parameter decoding, headers, media type; `withExtract` composes an extractor with a handler; `Middleware := Handler → Handler` |
| suspension | handlers are `VM` values; `GET /slow` parks 200 ms while a sibling request completes |
| middleware | `Handler → Handler`; `LeanIO.Backend.LeanIn.Middlewares` ports the four built-ins (`catchErrors`, `requestLogger`, `withExtension`, `auth`) with the same names/parameters, validators in `VM Bool`, and typed request extensions. `catchErrors` uses `VM.attempt`, whose catch survives a suspension (`/boom` parks then raises, answered 500) and whose 500 body is the error's `toString` |
| cancellation | a request's own transport failure — a failed body read, a stall, a peer close mid-body, a failed response send — is a typed VM abort, not a catchable raise: the streaming driver's request scope turns it into an `aborted` outcome, the driver closes the connection and reports the transport error, and attempts no further 500 or keep-alive; bytes already sent are not retracted, so no claim of zero written bytes is made |
| checks | `tests/leanio-native.sh` asserts routing, middleware, suspensions, bodies, keep-alive, methods and extraction statuses at the public socket; `tests/http-cancel-contract.sh HC1` and `HC2` assert the cancellation and the prefixed ordinary error at that socket |

Not yet ported: the route macros and `FromRequestBody`/`FromRequestParts` classes still ascribe
`ContextAsync`/`Request Body.Stream`, so the stock streaming APIs are later stages, as is response backpressure;
and so is the **rest** of the request scope — task/child cancellation, shutdown/drain and scope-exit cleanup. The
peer-I/O request scope and the streaming reader/writer themselves are implemented (the `bodies` and `cancellation`
rows above and the paragraph below). The fork builds and its own tests run under `lake build LeanIO`.

### The native backend's boundary

Handlers use the streaming session: `Request.body` is a `BodyReader` and the handler writes through a
`ResponseWriter`, so neither direction is accumulated unless the handler asks for it. `readAll` is what a
buffered extractor (`jsonBody`, `formBody`) calls: it accumulates the whole body with no byte-count check, so on
this path the accumulation is **not bounded by `H1.Limits.maxBody`** and may grow with the input. The `maxBody`
bound belongs to the separate buffered driver, which rejects an oversize `Content-Length` body with 413 before
dispatch. A route that streams calls `recv`/`write` itself and never accumulates. Responses are framed as a
declared `Content-Length`, chunked on HTTP/1.1, or close-delimited.

Route selection shares the route-declaration form. The unprefixed `GET … => …` selects the stock backend by
default and the native backend under `set_option leanio.backend.native true`; `native GET … => …` selects the
native backend regardless of the option. Under the native backend a body may be an `AppM` computation or a
response value, as in LeanIO: `=> "text"`, `=> Status.created`, `=> (Status.ok, value)` (text or JSON),
`=> (status × headers × value)`, `ByteArray`, `Except`, or `ResponseHead × ByteArray`; `RunBody` chooses the
value or computation form. Binders read through the native extractor classes: bodies `Json α`, `PlainText`,
`Form α`, `ByteArray`; parts `Path α` (from `FromPath` or `FromString`), `URI.Query`, `RequestTarget`, `URI.Path`, and Std `Method`,
`Version` and `Headers`. Native `Request` also exposes a stock-shaped `line : Std.Http.Request.Head`, so
middleware/handlers reading `req.line.method/uri/headers` need no accessor changes.
`nativeRoute GET "/ping" ping` takes an already-built handler.

Response types: native `IntoResponseM` is request-aware; `LeanIO.File` streams with Content-Length and an
ETag (304 on `If-None-Match`), and `LeanIO.BrowserCached` answers JSON with ETag/cache-control and 304.
`RangeFile` answers 206/Content-Range and 416 natively. `RemoteAddr`, the derive-from-* macros and `BodyExt`
are not ported.

Request lifecycle: each dispatched request runs under a `Scope` (in `Request.scope`). `AppM.onExit` registers
exactly-once cleanup, `AppM.spawn` starts a request-owned child, and `AppM.checkCancelled` aborts cooperatively;
the scope sets `cancelled` and runs cleanups after the handler's outcome. Cancellation is cooperative, as in
`ContextAsync`: a child that never checks continues. Shutdown/drain with a deadline is not implemented.

Remaining non-drop-in surfaces: a raw `(req : Request Body.Stream)` type annotation (the accessor shape is
present, but the type is the backend's `Request`), custom middleware written against
the stock `HandlerFn`/`ContextAsync` pair (the built-ins have native equivalents with the same names and
parameters), and the stock streaming request/response API. Native routes use the backend's
`Request`/`ResponseWriter` and `Middleware := Handler → Handler`; `Request` carries typed extensions.

Multipart is the upstream parser parameterised over its body source: `BodySource m body` with defaults
`ContextAsync`/`Body.Stream` keeps the stock API unchanged, and the native backend adds
`BodySource VM BodyReader`. `MultiPartForm`/`FormFile`/`MultipartEntry` are therefore the same types and
operations on both backends - `nextEntry`, `stream`, `discard`, `save` - and `withMultipartForm` runs them
over the native request reader. One parser serves both.

Cancellation is the transport's, and it is typed: `LeanIn.Runtime.VM` carries an abort channel separate from
`throwE`, `VM.abortOnFailure` classifies one raw completion's own failure as an abort, and the streaming driver's
`VM.requestScope` turns it into an `aborted` outcome (`docs/decisions.md` D23). `tests/http-cancel-contract.sh HC1`
drives a half-closed peer at the public socket: it reads zero post-close response bytes and the service answers a
fresh connection afterwards. The middleware no longer classifies by message text — an `Http:`-prefixed
**application** error is an ordinary raise, answered 500 with its `toString` body (`HC2`) — and the buffered
entrypoint has no scope, so an explicit abort in one of its handlers fails loudly rather than closing.

### Migrating an application to the native API

Route declarations and built-in middleware are source-compatible under `set_option leanio.backend.native true`;
three boundaries change:

| Stock | Native |
|---|---|
| `(req : Request Body.Stream)` | omit the annotation (inferred native `Request`) or name `LeanIO.Backend.LeanIn.Request`; `req.line.*`, `req.header?`, `req.path`, `req.extensions` are the same shape |
| `← req.body.readAll` in `ContextAsync` | `← req.body.readAll` in `VM` (`BodyReader.readAll`), or `req.body.recv n` for streaming |
| custom middleware `fun next req => do …` over `HandlerFn` | `fun next req w => do …` over `Request → ResponseWriter → VM Unit`; respond with `LeanIO.Backend.LeanIn.respond`/`w.start`/`w.write`/`w.finish` |
| stock streaming `Body.Stream`/`Response Body.Any` | `BodyReader.recv` and `ResponseWriter` chunks; responses are framed by the writer |

Built-ins (`catchErrors`, `requestLogger`, `withExtension`, `auth`) need only the import
`LeanIO.Backend.LeanIn.Middlewares`; auth validators run in `VM Bool`. Explicit stock class constraints
(`FromRequestBody`/`FromRequestParts`) become the native `...M` classes; binder syntax is unchanged.

## Working rules

- The fork builds together with the rest of the package: `nix develop -c lake build LeanIO`.
- Divergent edits are listed above as they are made; a sync from upstream must re-apply this log.
- Behavioural changes that affect LeanIO's public API are recorded here, not only in a commit message.
- The porting plan and the feature gaps it requires from LeanIn are in
  `docs/notes/LeanIO_to_LeanIn_Porting_Handoff.md`; the backend's effect signature is
  `docs/notes/LeanIO_to_LeanIn_Effect_Signature.md`.
- Run `nix develop -c bash tests/leanio-native.sh` after touching the backend or the HTTP driver.
