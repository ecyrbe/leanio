import Lean
import LeanIO.Utils

namespace LeanIO

/--
A zero-copy buffer of byte chunks. Chunks are oldest-first.
Implements `GetElem?` and `Search.Sized` so KMP search works directly on chunks,
with no flattening needed.
-/
structure ChunkBuffer where
  chunks : Array ByteArray
  total  : Nat

def ChunkBuffer.empty : ChunkBuffer :=
  { chunks := #[], total := 0 }

def ChunkBuffer.ofByteArray (ba : ByteArray) : ChunkBuffer :=
  { chunks := #[ba], total := ba.size }

def ChunkBuffer.append (cb : ChunkBuffer) (chunk : ByteArray) : ChunkBuffer :=
  { chunks := cb.chunks.push chunk, total := cb.total + chunk.size }

def ChunkBuffer.byteAt! (cb : ChunkBuffer) (i : Nat) : UInt8 :=
  go 0 i
where
  go (chunkIdx : Nat) (off : Nat) : UInt8 :=
    if h : chunkIdx < cb.chunks.size then
      let chunk := cb.chunks[chunkIdx]
      if off < chunk.size then
        chunk.get! off
      else
        go (chunkIdx + 1) (off - chunk.size)
    else
      panic! "byteAt! index out of bounds"

def ChunkBuffer.startsWithAt (cb : ChunkBuffer) (pos : Nat) (needle : ByteArray) : Bool :=
  if pos + needle.size > cb.total then false
  else Id.run do
    for i in [0:needle.size] do
      if cb.byteAt! (pos + i) ≠ needle.get! i then return false
    return true

def ChunkBuffer.toByteArray (cb : ChunkBuffer) : ByteArray :=
  cb.chunks.foldl (· ++ ·) ByteArray.empty

partial def ChunkBuffer.extract (cb : ChunkBuffer) (start : Nat) (stop : Nat) : ByteArray :=
  go 0 start (stop - start) ByteArray.empty
where
  go (chunkIdx : Nat) (off : Nat) (rem : Nat) (acc : ByteArray) : ByteArray :=
    if rem = 0 || chunkIdx ≥ cb.chunks.size then acc
    else
      let chunk := cb.chunks[chunkIdx]!
      if off ≥ chunk.size then
        go (chunkIdx + 1) (off - chunk.size) rem acc
      else
        let avail := min (chunk.size - off) rem
        go (chunkIdx + 1) 0 (rem - avail) (acc ++ chunk.extract off (off + avail))

instance : Search.Sized ChunkBuffer where
  size cb := cb.total

instance : GetElem? ChunkBuffer Nat UInt8 (λ cb i => i < cb.total) where
  getElem cb i _ := cb.byteAt! i
  getElem? cb i :=
    if i < cb.total then some (cb.byteAt! i) else none
  getElem! cb i := cb.byteAt! i

inductive SearchResult where
  | found     (pos : Nat)
  | notFound  (safe : Nat)
  | needMore

/--
Search `cb` for `s` starting at `start`. Returns:
- `.found pos` if found at `pos`
- `.notFound safe` if not found but `safe` bytes can be skipped (needle may start after)
- `.needMore` if more data from the stream is required
-/
def ChunkBuffer.searchSafe (cb : ChunkBuffer) (s : Search ChunkBuffer) (start : Nat) : SearchResult :=
  let available := cb.total - start
  if available = 0 then .needMore
  else
    match s.search cb start with
    | some dpos => .found dpos
    | none =>
      let overlap := min available (s.needle.total - 1)
      if available > overlap then .notFound (available - overlap)
      else .needMore

end LeanIO
