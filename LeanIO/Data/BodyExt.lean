module

public import Std.Http.Data.Body.Any
public import Std.Http.Data.Body.Stream
public import Std.Async

namespace Std.Http.Body

open Std.Async

/--
The monad in which body transforms run. Provides `get`/`set`
for state access and `lift` for `Async` operations.
-/
public abbrev TransformM (σ : Type) := StateT σ Async

/--
A stateful body chunk transformer.

Implement `transform` to define a stream-to-stream transformation
that holds internal state across chunks (e.g., compression,
encryption). Use `get`/`set` from `StateT` to access the state,
and `return` or `pure` to produce the output bytes.

```lean
structure GZipState where ...

instance : Transform GZipState where
  transform chunk := do
    let s ← get
    -- ... compress chunk using s ...
    set { s with ... }
    return compressedBytes
```

For stateless transforms, use a unit type :

```lean
struct ToUpper where
 -- no state

instance : Transform ToUpper where
  transform chunk := return toUpper chunk
```
-/
public class Transform (σ : Type) where
  /--
  Transforms a single chunk. Called sequentially for each chunk
  in the source body. Use `get`/`set` to read or update the state.
  -/
  transform (data : Chunk) : TransformM σ Chunk

/--
  No State sync transform instance using Chunk to Chunk
-/
public instance : Transform (Chunk → Chunk) where
  transform chunk f := pure (f chunk, f)

/--
  No State async transform instance using Chunk to Async Chunk
-/
public instance : Transform (Chunk → Async Chunk) where
  transform chunk f := return (← f chunk, f)


public partial def forIn
    {β : Type} [Http.Body α] (body : α) (acc : β)
    (step : Chunk → β → Async (ForInStep β)) : Async β := do
  loop body acc where
  @[specialize]
  loop (body : α) (acc : β) : Async β := do
    match ← Body.recv body with
    | some chunk =>
      match ← step chunk acc with
      | .done res => return res
      | .yield res => loop body res
    | none => return acc

/--
Iterates over every chunk in the body. Reads lazily via `recv`,
stopping when the body yields `none`.

```lean
for chunk in body do
  IO.println s!"got {chunk.data.size} bytes"
```
-/
public instance [Http.Body α] : ForIn Async α Chunk where
  forIn := forIn

/--
Pipes the body data through a stateful `Transform`,
returning a new `Body.Any` backed by a stream.

A background task reads chunks from `self`, applies `transform`
sequentially, and sends results to the output stream.

```lean
let compressed ← res.body.pipeThrough initialCompressState
return { res with body := compressed }
```
-/
@[specialize]
public def pipeThrough [Transform σ] [Http.Body α] (self : α) (initial : σ) : Async Body.Stream :=
  Body.stream fun outStream => do
    let mut state := initial
    for chunk in self do
      let (transformed, nextState) ← Transform.transform chunk state
      outStream.send transformed
      state := nextState

end Std.Http.Body
