import Lean
import LeanIO.Utils

namespace Std.Slice

@[inline]
def toByteArrayFast (slice: ByteSlice): ByteArray :=
  if slice.start == 0 && slice.size == slice.byteArray.size then
    slice.byteArray
  else
    slice.toByteArray

end Std.Slice

namespace LeanIO

/--
A zero-copy buffer of byte chunks. Chunks are oldest-first.
Implements `GetElem?` and `Search.Sized` so KMP search works directly on chunks,
with no flattening needed.
-/
structure ChunkBuffer where
  chunks : Array ByteSlice
  size  : Nat

namespace ChunkBuffer


@[inline]
def empty : ChunkBuffer :=
  { chunks := #[], size := 0 }

@[inline]
def ofByteArray (ba : ByteArray) : ChunkBuffer :=
  { chunks := #[ba.toByteSlice], size := ba.size }

@[inline]
def ofSlice (s: ByteSlice) : ChunkBuffer :=
  { chunks := #[s], size := s.size }

@[inline]
def add (self : ChunkBuffer) (chunk : ByteArray) : ChunkBuffer :=
  { chunks := self.chunks.push chunk.toByteSlice, size := self.size + chunk.size }

@[inline]
def addSlice (self: ChunkBuffer) (s: ByteSlice) : ChunkBuffer :=
  { chunks := self.chunks.push s, size := self.size + s.size }

@[inline]
def byteAt! (self : ChunkBuffer) (i : Nat) : UInt8 :=
  go 0 i
where
  go (chunkIdx : Nat) (off : Nat) : UInt8 :=
    if h : chunkIdx < self.chunks.size then
      let chunk := self.chunks[chunkIdx]
      if h: off < chunk.size then
        chunk.get ⟨off,h⟩
      else
        go (chunkIdx + 1) (off - chunk.size)
    else
      panic! "byteAt! index out of bounds"

@[inline]
def startsWithAt (self : ChunkBuffer) (pos : Nat) (needle : ByteArray) : Bool :=
  if pos + needle.size > self.size then false
  else Id.run do
    for i in [0:needle.size] do
      if self.byteAt! (pos + i) ≠ needle.get! i then return false
    return true

@[inline]
def toByteArray (self : ChunkBuffer) : ByteArray :=
  self.chunks.foldl (· ++ ·.toByteArrayFast) ByteArray.empty

partial def extract (self : ChunkBuffer) (start : Nat) (stop : Nat) : ChunkBuffer :=
  go 0 start (stop - start) ChunkBuffer.empty
where
  go (chunkIdx : Nat) (off : Nat) (rem : Nat) (acc : ChunkBuffer) : ChunkBuffer :=
    if rem = 0 || chunkIdx ≥ self.chunks.size then acc
    else
      let chunk := self.chunks[chunkIdx]!
      if off ≥ chunk.size then
        go (chunkIdx + 1) (off - chunk.size) rem acc
      else
        let avail := min (chunk.size - off) rem
        go (chunkIdx + 1) 0 (rem - avail) (acc.addSlice <| chunk.slice off (off + avail))

instance : Search.Sized ChunkBuffer where
  size := ChunkBuffer.size

instance : GetElem? ChunkBuffer Nat UInt8 (λ cb i => i < cb.size) where
  getElem cb i _ := cb.byteAt! i
  getElem? cb i :=
    if i < cb.size then some (cb.byteAt! i) else none
  getElem! cb i := cb.byteAt! i

inductive SearchResult where
  | found     (pos : Nat)
  | notFound  (safe : Nat)
  | needMore

/--
Search `cb` for `s` starting at `start`. Returns:
- `.found pos` if found at `pos`
- `.notFound safe` if not found but `safe` bytes can be safely emitted (needle may start after)
- `.needMore` if more data from the stream is required
-/
def searchSafe (cb : ChunkBuffer) (s : Search ChunkBuffer) (start : Nat) : SearchResult :=
  let available := cb.size - start
  if available = 0 then .needMore
  else
    match s.search cb start with
    | some dpos => .found dpos
    | none =>
      let overlap := s.terminalOverlap cb start
      if available > overlap then .notFound (available - overlap)
      else .needMore

end ChunkBuffer

end LeanIO
