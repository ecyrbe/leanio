import LeanIO.Backend.LeanIn

/-!
# `leanioCancelContract` — the HTTP cancellation contract's service

Serves a native router with its `catchErrors` middleware at a local loopback socket. Test-only routes are
declared here over the production router API: an ordinary `/ping`, a `/boom` that raises an ordinary error
immediately, a `POST /cancel-probe` that accumulates the request body inside `VM.attempt`, and — for the
prefixed-error check — `/prefixed-error`, `/error-text`, `/ready` and `/release`.

A client that declares a body, receives the interim `100 Continue`, sends part of it and then half-closes
its write side can therefore observe whether the peer-close read failure reached the catch as an ordinary
error or bypassed it. A client of `/prefixed-error` can park a handler's `ioE` leaf, observe through `/ready`
that its success continuation was registered, release it through `/release`, and compare what the waiting
connection receives with the `/error-text` serialization of the same `IO.Error` value.

`tests/http-cancel-contract.sh` starts this executable and drives both at the public loopback socket.
-/

namespace LeanIO.Test.HttpCancelContract

open LeanIn

/-- `GET /ping`: an ordinary immediate answer, and the recovery control's route. -/
def ping : LeanIO.Backend.LeanIn.Handler :=
  fun _ w => LeanIO.Backend.LeanIn.respond w 200 "OK" "pong\n".toUTF8

/-- `GET /boom`: raise an ordinary error at once — no suspension, no clock, no timer — so the `catchErrors`
middleware answers it with a 500. This is the status-line detector's affirmative control. -/
def boom : LeanIO.Backend.LeanIn.Handler :=
  fun _ _ => throw (IO.userError "boom: an ordinary handler error")

/-- `POST /cancel-probe`: read the body inside `VM.attempt`. An ordinary failure answers a distinctive
`500`, so a cancellation the attempt can see becomes a response; a cancellation it cannot see leaves
nothing for the catch to send. -/
def cancelProbe : LeanIO.Backend.LeanIn.Handler :=
  fun req w => do
    match ← LeanIn.Runtime.VM.attempt (LeanIO.Backend.LeanIn.readAll req.body .empty) with
    | .ok body => LeanIO.Backend.LeanIn.respond w 200 "OK" body
    | .error _ => LeanIO.Backend.LeanIn.respond w 500 "Caught Cancellation" "caught\n".toUTF8

/-- The one named error value the parked route raises and the reference route serializes. It is an ordinary
application error whose `toString` begins `Http:`; that prefix is incidental text, not a transport failure,
and the middleware's ordinary error catch answers it with a 500 whose body is that same `toString`. -/
def prefixedError : IO.Error := IO.userError "Http: prefixed application sentinel"

/-- `GET /error-text`: the expected body, as `toString` of the same error value the parked route raises. -/
def errorText : LeanIO.Backend.LeanIn.Handler :=
  fun _ w => LeanIO.Backend.LeanIn.respond w 200 "OK" (toString prefixedError).toUTF8

/-- The service's one-shot completion state: at most one parked `ioE` success continuation. -/
structure Control where
  pending : IO.Ref (Option (Unit → IO Unit))

/-- `GET /prefixed-error`: park an `ioE` leaf by storing its success continuation and returning without
invoking either continuation. `catchErrors` is waiting on this machine, so the continuation resumes the
middleware's error scope when `/release` invokes it, and the handler then raises `prefixedError`. -/
def prefixedErrorRoute (ctl : Control) : LeanIO.Backend.LeanIn.Handler :=
  fun _ _ => do
    let park : (Unit → IO Unit) → (IO.Error → IO Unit) → IO Unit :=
      fun kok _kerr => (ctl.pending.set (some kok) : IO Unit)
    LeanIn.Runtime.VM.ioE park
    throw prefixedError

/-- `GET /ready`: whether a success continuation is currently parked. -/
def readyRoute (ctl : Control) : LeanIO.Backend.LeanIn.Handler :=
  fun _ w => do
    let pending ← (monadLift (ctl.pending.get : IO (Option (Unit → IO Unit))) :
      LeanIn.Runtime.VM (Option (Unit → IO Unit)))
    if pending.isSome then
      LeanIO.Backend.LeanIn.respond w 200 "OK" "ready\n".toUTF8
    else
      LeanIO.Backend.LeanIn.respond w 200 "OK" "waiting\n".toUTF8

/-- `GET /release`: take the parked success continuation out of the state, so it can be invoked exactly
once, and resolve it. Only this route invokes the success continuation; the error continuation is never
called. -/
def releaseRoute (ctl : Control) : LeanIO.Backend.LeanIn.Handler :=
  fun _ w => do
    let pending : Option (Unit → IO Unit) ←
      (monadLift (do
        let p ← (ctl.pending.get : IO (Option (Unit → IO Unit)))
        (ctl.pending.set none : IO Unit)
        pure p) : LeanIn.Runtime.VM (Option (Unit → IO Unit)))
    match pending with
    | some kok =>
      monadLift (kok ())
      LeanIO.Backend.LeanIn.respond w 200 "OK" "released\n".toUTF8
    | none =>
      LeanIO.Backend.LeanIn.respond w 409 "Conflict" "nothing pending\n".toUTF8

/-- The router: the test-only routes and the production `catchErrors` middleware. `toTrie` composes the
middleware once, so every route sits inside the same error scope. -/
def routes (ctl : Control) : LeanIO.Backend.LeanIn.NativeRouter :=
  LeanIO.Backend.LeanIn.NativeRouter.empty
    |>.route .get "/ping" ping
    |>.route .get "/boom" boom
    |>.route .post "/cancel-probe" cancelProbe
    |>.route .get "/prefixed-error" (prefixedErrorRoute ctl)
    |>.route .get "/error-text" errorText
    |>.route .get "/ready" (readyRoute ctl)
    |>.route .get "/release" (releaseRoute ctl)
    |>.addMiddleware LeanIO.Backend.LeanIn.catchErrors

/-- Serve the routes on `port`, the first argument or `8791`. -/
def run (args : List String) : IO UInt32 := do
  let port : UInt16 := ((args.headD "8791").toNat?.getD 8791).toUInt16
  let pending ← (monadLift (IO.mkRef (none : Option (Unit → IO Unit))) :
    IO (IO.Ref (Option (Unit → IO Unit))))
  let ctl : Control := { pending }
  let hooks ← LeanIn.Runtime.Hooks.new
  LeanIO.Backend.LeanIn.serveWith hooks (LeanIO.Backend.LeanIn.dispatch (routes ctl).toTrie) port

end LeanIO.Test.HttpCancelContract

/-- Entry point for `leanioCancelContract`. -/
def main (args : List String) : IO UInt32 := LeanIO.Test.HttpCancelContract.run args
