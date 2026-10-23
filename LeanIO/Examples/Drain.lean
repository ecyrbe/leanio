import LeanIO.Backend.LeanIn
import LeanIO.Router.Macro

/-!
# A native server with a stop handle

`lake exe leanioDrain <port> <stop-after-ms>` starts the native server, serves until the delay elapses, then
stops admission and drains in-flight connections before exiting. The route `/slow` parks for 200 ms so a request
can be in flight when the stop is requested.
-/

open LeanIn
open LeanIO.Backend.LeanIn

def routes (clock : LeanIn.Runtime.Clock) : NativeRouter :=
  NativeRouter.empty
    |>.route .get "/slow" (fun _ w => do
      let _ ← LeanIn.Runtime.VM.sleep clock 300
      LeanIO.Backend.LeanIn.respond w 200 "OK" "slow\n".toUTF8)
    |>.route .get "/ping" (fun _ w =>
      LeanIO.Backend.LeanIn.respond w 200 "OK" "pong\n".toUTF8)

def main (args : List String) : IO UInt32 := do
  let port : UInt16 := ((args[0]?.getD "8822").toNat?.getD 8822).toUInt16
  let stopAfter : Nat := ((args[1]?.getD "600").toNat?.getD 600)
  let hooks ← LeanIn.Runtime.Hooks.new
  let clock := LeanIn.Runtime.Clock.live hooks
  let h ← serveWithHandle hooks (dispatch (routes clock).toTrie) port
  IO.println "ready"
  IO.sleep stopAfter.toUInt32
  h.stop
  let outcome ← h.join
  LeanIn.Runtime.Io.stop
  match outcome with
  | .ok _ => IO.println "drained"; return 0
  | .error e => IO.println s!"serve ended: {e}"; return 1
