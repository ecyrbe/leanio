import Std.Async.ContextAsync
import Std.Data.ByteSlice
import LeanIO.Data.ChunkBuffer
import LeanIO.Request.MultiPartForm.Defs
import LeanIO.Request.MultiPartForm.Headers

namespace LeanIO
open Std.Http Std.Async Std.Slice

/-- Emit each slice of `body` to `cb`, converting via `toByteArrayFast`. -/
@[inline]
def emitChunks (body : ChunkBuffer) (cb : ByteArray → ContextAsync Unit) : ContextAsync Unit := do
    for chunk in body.chunks do
      cb chunk.toByteArrayFast

def readMore (inner : IO.Ref MultipartInner) : ContextAsync Bool := do
  let st ← inner.get
  if st.pos ≥ 65536 then
    inner.modify fun s => { s with cb := st.cb.extract st.pos st.cb.size, pos := 0 }
  let st ← inner.get
  match ← st.stream.recv with
  | none => return false
  | some chunk =>
    inner.modify fun s => { s with cb := s.cb.add chunk.data }
    return true

partial def readUntilGo (inner : IO.Ref MultipartInner) (delimSearch : Search ChunkBuffer) (startPos : Nat) : ContextAsync (ChunkBuffer × Nat) := do
  let st ← inner.get
  match st.cb.searchSafe delimSearch st.pos with
  | .found dpos =>
    let body := st.cb.extract st.pos dpos
    let newPos := dpos + delimSearch.needle.size
    inner.modify fun s => { s with pos := newPos }
    return (body, newPos)
  | .notFound safe =>
    inner.modify fun s => { s with pos := st.pos + safe }
    readUntilGo inner delimSearch startPos
  | .needMore =>
    if ← readMore inner then
      readUntilGo inner delimSearch startPos
    else
      let total := st.cb.size
      let body := st.cb.extract startPos total
      inner.modify fun s => { s with pos := total }
      return (body, total)

def readUntil (inner : IO.Ref MultipartInner) (delimSearch : Search ChunkBuffer) : ContextAsync ByteArray := do
  let st ← inner.get
  let (body, _) ← readUntilGo inner delimSearch st.pos
  return body.toByteArray

partial def skip (inner : IO.Ref MultipartInner) (pref : ByteArray) : ContextAsync Unit := do
  let st ← inner.get
  if st.cb.startsWithAt st.pos pref then
    inner.modify fun s => { s with pos := s.pos + pref.size }
  else
    let ok ← readMore inner
    if ok then skip inner pref

/-- Split `dataCB` at `boundSearch`. If found: emit body slices, store rest. Otherwise:
    emit safe portion and pass tail/remainder to `k`. -/
def emitBoundary (inner : IO.Ref MultipartInner) (boundSearch : Search ChunkBuffer)
    (dataCB : ChunkBuffer) (cb : ByteArray → ContextAsync Unit)
    (k : ChunkBuffer → ContextAsync Unit) : ContextAsync Unit := do
  let dataLen := dataCB.size
  match dataCB.searchSafe boundSearch 0 with
  | .found dpos =>
    let needleSz := boundSearch.needle.size
    emitChunks (dataCB.extract 0 dpos) cb
    inner.modify fun s => { s with cb := dataCB.extract (dpos + needleSz) dataLen, pos := 0, phase := .ready }
  | .notFound safe =>
    emitChunks (dataCB.extract 0 safe) cb
    k (dataCB.extract safe dataLen)
  | .needMore =>
    k dataCB

/-- Stream raw chunks from the stream. No buffering in ChunkBuffer. -/
partial def streamRaw (inner : IO.Ref MultipartInner) (boundSearch : Search ChunkBuffer)
    (cb : ByteArray → ContextAsync Unit) (carry : ChunkBuffer) : ContextAsync Unit := do
  let st ← inner.get
  let chunkOpt ← st.stream.recv
  match chunkOpt with
  | none =>
    if carry.size > 0 then
      emitBoundary inner boundSearch carry cb fun rest => do
        emitChunks rest cb
    inner.modify fun s => { s with phase := .ready }
  | some chunk =>
    let dataLen := carry.size + chunk.data.size
    emitBoundary inner boundSearch (carry.add chunk.data) cb fun rest => do
      if rest.size = dataLen then
        emitChunks rest cb
        streamRaw inner boundSearch cb ChunkBuffer.empty
      else
        streamRaw inner boundSearch cb rest

def startStreamFile (f : FormFile) (cb : ByteArray → ContextAsync Unit) : ContextAsync Unit := do
  let st ← f.inner.get
  f.inner.set { st with cb := ChunkBuffer.empty, pos := 0 }
  streamRaw f.inner st.boundSepSearch cb (st.cb.extract st.pos st.cb.size)

end LeanIO
