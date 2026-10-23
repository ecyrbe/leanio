# HTTP cancellation — behaviour checks

A request that declares a body and then half-closes its write side before sending it is the socket's own
cancellation signal. The checks here drive that at the public loopback socket of a served HTTP/1.1
connection and observe what the peer reads: whether a handler's catch can turn the peer-close read failure
into a response, and whether the connection and the service complete afterwards.

```sh
nix develop -c bash tests/http-cancel-contract.sh HC1
nix develop -c bash tests/http-cancel-contract.sh HC2
```

The fixture is `tests/http-cancel-contract.sh`. It builds and starts `leanioCancelContract`
(`LeanIn/Test/HttpCancelContract.lean`), which declares test-only routes over the production
`NativeRouter`/`dispatch` API and attaches the production `catchErrors` middleware: an ordinary `/ping`, a
`/boom` that raises an ordinary error immediately, the `POST /cancel-probe` that reads the request body, and
for the prefixed-error check `/prefixed-error`, `/error-text`, `/ready` and `/release`.
The socket is real and local — the service binds the loopback address itself — so the subject is the
connection's response, not an internal `VM` call. No external service and no faked `Conn` is involved
(`LeanIn/Runtime/Io.lean` keeps its constructor private), and no clock is asserted: the fixture's timeouts
only bound a wait.

A failure before a scenario's own assertion — the build, the executable, startup, readiness, the interim
`100 Continue`, the `/boom` control, a bounded read, or a reset standing in for end of stream — is reported
as a setup error with exit 2, separately from a scenario assertion, which exits 1. A bounded wait is never
the scenario's result.

## HC1 — a peer-close body read bypasses catch/middleware and completes the connection

**Actor.** An HTTP client that connects to the in-test `leanioCancelContract` service. An ordinary `/boom`
client and a `/ping` client in the same invocation are its detector and recovery controls.

**Boundary.** The service's public loopback socket and the HTTP response a client reads there.

**Given.** The service composes a native router and attaches the existing `catchErrors` middleware. Its
test-only `POST /cancel-probe` handler wraps `LeanIO.Backend.LeanIn.readAll req.body .empty` in `VM.attempt`;
on `.error` that handler answers a distinctive `500 Caught Cancellation`, on `.ok` an ordinary 200. The same
router serves `/ping` (`pong\n`) and `/boom`, which raises an ordinary error immediately — no suspension and
no clock — so `catchErrors` answers it 500; the invocation's ordinary `/boom` GET is the status-line
detector's affirmative control. The cancellation client opens its own connection, sends
`POST /cancel-probe` with `Expect: 100-continue` and `Content-Length: 10`, and reads and validates the
interim `100 Continue`, so the parsed head has reached the streaming handler.

**When.** The client sends a partial body — 4 of the 10 declared bytes — and then shuts down its write side
(FIN), keeping its read side open.

**Then.** The client receives **zero** response bytes after the interim 100 and then a readable end of
stream: no status line, so no 500 from the probe's caught branch and none from `catchErrors`. In the same
invocation the `/boom` connection was parsed as status 500 by the same status-line detector that finds no
status here, and a fresh `/ping` connection after the cancelled one is answered 200 with the exact body
`pong\n`.

**Why.** A caught-branch reply is a response the peer can still read, so the zero-byte outcome is what
distinguishes a cancellation that bypassed `VM.attempt` from an ordinary error it converted. The ordinary
500 on `/boom` shows the detector can see a status line, and `/ping` shows the cancelled request's
completion did not wedge the service. This states the body-read half only; it does not claim arbitrary
worker or child cancellation.

## HC2 — an ordinary `Http:` error after a genuine park receives the detailed 500 and its message bytes

**Actor.** An HTTP client of the in-test `leanioCancelContract` service that holds `/prefixed-error` open
while it drives the readiness handshake, with a `/ping` client in the same invocation as its recovery
control.

**Boundary.** The service's public loopback socket and the HTTP responses a client reads there.

**Given.** The service composes the same native router and attaches the production `catchErrors`
middleware. Its `GET /prefixed-error` handler parks a VM `ioE` leaf by storing its success continuation in
the service's `IO.Ref` and returning without invoking either continuation; `GET /ready` reports whether
that continuation is parked, and `GET /release` takes it out of the state and invokes it exactly once,
never the error continuation. The resumed handler raises one named `IO.Error` value,
`IO.userError "Http: prefixed application sentinel"`, and the independent `GET /error-text` route answers
200 with `toString` of that same value as its body. Reading whether the callback was parked, and the
`/ready` and `/release` responses themselves, are the fixture's synchronization rather than the
scenario's assertion; a stalled handshake is a setup error.

**When.** The client opens `/prefixed-error` with `Connection: close`, waits until `/ready` reports the
parked continuation, releases it once through `/release`, and reads the response the waiting connection
receives.

**Then.** The waiting connection's first response parses as **status 500**, its parsed
`Content-Length` agrees with the number of body bytes received, and those body bytes are exactly the bytes
`/error-text` returned for the same error value. The connection completes at a readable end of stream, and
a fresh `/ping` connection is answered 200 with the exact body `pong\n`.

**Why.** The `Http:` prefix is incidental text, not the transport boundary: peer-close cancellation goes
by the separate VM abort and request scope and never enters `catchErrors`'s ordinary error path, while
this application error does and receives the usual detailed 500 with its own `toString` as the body;
binding that body to an independently served serialization is what the status line alone cannot show.
