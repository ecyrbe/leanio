import Std.Async.ContextAsync
import LeanIO.Response.IntoResponse
import LeanIO.Response.File.Utils
import LeanIO.Request.HeaderRange
import LeanIO.Data.MimeType
import LeanIO.Data.HeaderName

namespace LeanIO
open Std.Http Std.Async

private def headerBytes        : Header.Value := Header.Value.mk "bytes"

/--
A file on disk served with optional `Range` support for efficient seeking.

returns the appropriate `206 Partial Content` with `Content-Range` for
range requests, or the full file otherwise.

`IntoResponse` is implemented with streaming and `Content-Length` framing
(not chunked transfer encoding).

```lean
def serveFile := GET "/static/{*rest}" (⟨rest⟩ : Path String) => do
  return { path := "static" / rest : RangeFile }
```
-/
structure RangeFile where
  path   : System.FilePath

private def chunkSize : Nat := 8192

private def pickRange (ranges : Option (Array Range)) (fileSize : Nat) : Option (Nat × Nat) :=
  match ranges with
  | none => none
  | some rs =>
    if _ : rs.size > 0 then
      let r := rs[0]!
      let (start, len) := match r.start, r.stop with
        | some s, some e =>
          if s >= fileSize then (0, 0) else
          let e := min e (fileSize - 1)
          (s, e - s + 1)
        | some s, none =>
          if s >= fileSize then (0, 0) else
          (s, fileSize - s)
        | none, some suffix =>
          let suffix := min suffix fileSize
          (fileSize - suffix, suffix)
        | none, none => (0, fileSize)
      some (start, len)
    else none

private def skipBytes (handle : IO.FS.Handle) (n : Nat) : IO Unit := do
  let mut skipped := 0
  while skipped < n do
    let bytes ← handle.read (USize.ofNat (min chunkSize (n - skipped)))
    if bytes.isEmpty then break
    skipped := skipped + bytes.size

instance : IntoResponseExt RangeFile where
  into_response_ext req f := do
    let file ← f
    if !(←file.path.pathExists) || (←file.path.isDir) then
      Response.notFound |>.empty
    else
      let mdata ← file.path.metadata
      let fileSize := mdata.byteSize.toNat
      let handle ← IO.FS.Handle.mk file.path .read
      let ranges := req.line.headers.get? .range |>.bind (parseRange ·.value)
      match pickRange ranges fileSize with
      | none =>
        Response.ok
          |>.header .contentType (MimeType.mimeType file.path)
          |>.header .acceptRanges headerBytes
          |>.stream (sendFileStream handle fileSize)
      | some (start, len) =>
        if start >= fileSize then
          Response.new.status .rangeNotSatisfiable
            |>.header! "content-range" s!"bytes */{mdata.byteSize}"
            |>.empty
        else
          skipBytes handle start
          let endByte := start + len - 1
          Response.new.status .partialContent
            |>.header .contentType (MimeType.mimeType file.path)
            |>.header .acceptRanges headerBytes
            |>.header! "content-range" s!"bytes {start}-{endByte}/{mdata.byteSize}"
            |>.stream (sendFileStream handle len)

end LeanIO
