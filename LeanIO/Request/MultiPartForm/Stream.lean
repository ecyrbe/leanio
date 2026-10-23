module

public import Std.Async.ContextAsync
import Std.Data.ByteSlice
public import LeanIO.Utils
public import LeanIO.Data.ChunkBuffer
public import LeanIO.Request.MultiPartForm.Defs
import LeanIO.Request.MultiPartForm.Headers

namespace LeanIO
open Std.Http Std.Async Std.Slice

variable {m : Type → Type} {body : Type} [Monad m] [MonadLift IO m] [BodySource m body]
  [Inhabited (m Unit)] [Inhabited (m (ChunkBuffer × Nat))]

/-- Emit each slice of `body` to `cb`, converting via `toByteArrayFast`. -/
@[inline]
public def emitChunks (buf : ChunkBuffer) (cb : ByteArray → m Unit) : m Unit := do
    for chunk in buf.chunks do
      cb chunk.toByteArrayFast

public def readMore (inner : IO.Ref (MultipartInner m body)) : m Bool := do
  let st ← refGet inner
  if st.pos ≥ 65536 then
    refModify inner fun s => { s with cb := st.cb.extract st.pos st.cb.size, pos := 0 }
  let st ← refGet inner
  match ← BodySource.recv (m := m) st.stream with
  | none => return false
  | some chunk =>
    refModify inner fun s => { s with cb := s.cb.add chunk }
    return true

public partial def readUntilGo [Inhabited (m (ChunkBuffer × Nat))]
    (inner : IO.Ref (MultipartInner m body)) (delimSearch : Search ChunkBuffer) (startPos : Nat) : m (ChunkBuffer × Nat) := do
  let st ← refGet inner
  match st.cb.searchSafe delimSearch st.pos with
  | .found dpos =>
    let body := st.cb.extract startPos dpos
    let newPos := dpos + delimSearch.needle.size
    refModify inner fun s => { s with pos := newPos }
    return (body, newPos)
  | .notFound safe =>
    refModify inner fun s => { s with pos := st.pos + safe }
    readUntilGo inner delimSearch startPos
  | .needMore =>
    if ← readMore inner then
      readUntilGo inner delimSearch startPos
    else
      let total := st.cb.size
      let body := st.cb.extract startPos total
      refModify inner fun s => { s with pos := total }
      return (body, total)

public def readUntil (inner : IO.Ref (MultipartInner m body)) (delimSearch : Search ChunkBuffer) : m ByteArray := do
  let st ← refGet inner
  let (body, _) ← readUntilGo inner delimSearch st.pos
  return body.toByteArray

public partial def skip [Inhabited (m Unit)]
    (inner : IO.Ref (MultipartInner m body)) (pref : ByteArray) : m Unit := do
  let st ← refGet inner
  if st.cb.startsWithAt st.pos pref then
    refModify inner fun s => { s with pos := s.pos + pref.size }
  else
    let ok ← readMore inner
    if ok then skip inner pref

/-- Split `dataCB` at `boundSearch`. If found: emit body slices, store rest. Otherwise:
    emit safe portion and pass tail/remainder to `k`. -/
public def emitBoundary (inner : IO.Ref (MultipartInner m body)) (boundSearch : Search ChunkBuffer)
    (dataCB : ChunkBuffer) (cb : ByteArray → m Unit)
    (k : ChunkBuffer → m Unit) : m Unit := do
  let dataLen := dataCB.size
  match dataCB.searchSafe boundSearch 0 with
  | .found dpos =>
    let needleSz := boundSearch.needle.size
    emitChunks (dataCB.extract 0 dpos) cb
    refModify inner fun s => { s with cb := dataCB.extract (dpos + needleSz) dataLen, pos := 0, phase := .ready }
  | .notFound safe =>
    emitChunks (dataCB.extract 0 safe) cb
    k (dataCB.extract safe dataLen)
  | .needMore =>
    k dataCB

/-- Stream raw chunks from the stream. No buffering in ChunkBuffer. -/
public partial def streamRaw [Inhabited (m Unit)] (inner : IO.Ref (MultipartInner m body))
    (boundSearch : Search ChunkBuffer) (cb : ByteArray → m Unit) (carry : ChunkBuffer) : m Unit := do
  let st ← refGet inner
  let chunkOpt ← BodySource.recv (m := m) st.stream
  match chunkOpt with
  | none =>
    if carry.size > 0 then
      emitBoundary inner boundSearch carry cb fun rest => do
        emitChunks rest cb
    refModify inner fun s => { s with phase := .ready }
  | some chunk =>
    let dataLen := carry.size + chunk.size
    emitBoundary inner boundSearch (carry.add chunk) cb fun rest => do
      if rest.size = dataLen then
        emitChunks rest cb
        streamRaw inner boundSearch cb ChunkBuffer.empty
      else
        streamRaw inner boundSearch cb rest

public def startStreamFile (f : FormFile m body) (cb : ByteArray → m Unit) : m Unit := do
  let st ← refGet f.inner
  refSet f.inner { st with cb := ChunkBuffer.empty, pos := 0 }
  streamRaw f.inner st.boundSepSearch cb (st.cb.extract st.pos st.cb.size)

end LeanIO
