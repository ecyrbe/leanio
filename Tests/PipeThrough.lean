import Std.Http.Data.Body.Any
import Std.Http.Data.Body.Stream
import LeanIO.Data.BodyExt

open Std.Http
open Std.Async

private def toUpper (chunk : Chunk) : Chunk := Id.run do
  let mut data := ByteArray.empty
  for i in [:chunk.data.size] do
    let b := chunk.data.get! i
    data := data.push (if 97 ≤ b && b ≤ 122 then b - 32 else b)
  return {data}

/-- Stateful: counts chunks and prepends a counter to each. -/
private structure WithCounter where
  count : Nat

private instance : Body.Transform WithCounter where
  transform data := do
    let s ← get
    let label : ByteArray := ("[" ++ toString s.count ++ "] ").toUTF8
    set { s with count := s.count + 1 }
    return { data:= label ++ data.data}

private def drain (body : Body.Any) : Async ByteArray := do
  let mut result := ByteArray.empty
  repeat
    match ← body.recv with
    | some chunk => result := result ++ chunk.data
    | none => break
  return result

def main : IO Unit := Async.block do

  -- === Test 1: Unit (stateless) ===
  IO.println "--- test 1: Unit (stateless) ---"
  let input : ByteArray := "hello world".toUTF8
  let srcStream ← Body.fromBytes input
  let srcAny := Body.Any.ofBody srcStream

  let piped ← Std.Http.Body.pipeThrough srcAny toUpper
  let result ← drain piped
  IO.println s!"  input:  '{String.fromUTF8! input}'"
  IO.println s!"  output: '{String.fromUTF8! result}'"
  assert! String.fromUTF8! result == "HELLO WORLD"
  IO.println "  PASS"

  -- === Test 2: Unit (streaming) ===
  IO.println "--- test 2: Unit (streaming chunks) ---"
  let srcStream ← Body.stream fun s => do
    s.send { data := "hel".toUTF8 }
    s.send { data := "lo ".toUTF8 }
    s.send { data := "wor".toUTF8 }
    s.send { data := "ld".toUTF8 }
  let srcAny := Body.Any.ofBody srcStream

  let piped ← Std.Http.Body.pipeThrough srcAny toUpper
  let result ← drain piped
  IO.println s!"  input:  'hello world' (4 chunks)"
  IO.println s!"  output: '{String.fromUTF8! result}'"
  assert! String.fromUTF8! result == "HELLO WORLD"
  IO.println "  PASS"

  -- === Test 3: WithCounter (stateful) ===
  IO.println "--- test 3: WithCounter (stateful) ---"
  let srcStream ← Body.stream fun s => do
    s.send { data := "a".toUTF8 }
    s.send { data := "b".toUTF8 }
    s.send { data := "c".toUTF8 }
  let srcAny := Body.Any.ofBody srcStream

  let piped ← Std.Http.Body.pipeThrough srcAny { count := 0 : WithCounter }
  let result ← drain piped
  IO.println s!"  input:  'a' 'b' 'c' (3 chunks)"
  IO.println s!"  output: '{String.fromUTF8! result}'"
  assert! String.fromUTF8! result == "[0] a[1] b[2] c"
  IO.println "  PASS"

  -- === Test 4: empty body ===
  IO.println "--- test 4: empty body ---"
  let srcStream ← Body.empty
  let srcAny := Body.Any.ofBody srcStream

  let piped ← Std.Http.Body.pipeThrough srcAny toUpper
  let result ← drain piped
  IO.println "  input:  (empty)"
  IO.println "  output: (empty)"
  assert! result.isEmpty
  IO.println "  PASS"

  IO.println ""
  IO.println "All tests passed."
