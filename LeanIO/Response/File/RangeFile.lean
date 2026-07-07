import Std.Async.ContextAsync
import LeanIO.Response.Common
import LeanIO.Response.IntoResponse
import LeanIO.Response.File.Utils
import LeanIO.Request.HeaderRange
import LeanIO.Data.Headers.MimeType
import LeanIO.Data.Headers.HeaderName
import LeanIO.Data.Headers.CacheControl

namespace LeanIO
open Std.Http Std.Async

private def headerBytes : Header.Value := Header.Value.mk "bytes"

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
  new ::
  path         : System.FilePath
  cacheControl : Option CacheControl := some <| .publicStatic 0
deriving Inhabited

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

instance : IntoResponseExt RangeFile where
  into_response_ext req f := do
    let file ← f
    if !(←file.path.pathExists) || (←file.path.isDir) then
      Response.notFound |>.empty
    else
      let mdata ← file.path.metadata
      let fileSize := mdata.byteSize.toNat
      match file.cacheControl with
      | some cacheControl =>
        let etag := computeETag mdata
        if etagMatches req etag then
          Response.new |>.status Status.notModified |>.empty
        else
          let handle ← IO.FS.Handle.mk file.path .read
          let ranges := req.line.headers.get? .range |>.bind (parseRange ·.value)
          let baseResp := Response.ok
            |>.header .contentType (MimeType.mimeType file.path)
            |>.header .acceptRanges headerBytes
            |>.header .etag etag
            |>.header .cacheControl cacheControl
          match pickRange ranges fileSize with
          | none =>
            baseResp |>.stream (sendFileStream handle fileSize)
          | some (start, len) =>
            if start >= fileSize then
              Response.new.status .rangeNotSatisfiable
                |>.header! "content-range" s!"bytes */{mdata.byteSize}"
                |>.empty
            else
              skipBytes handle start
              let endByte := start + len - 1
              baseResp
                |>.status .partialContent
                |>.header! "content-range" s!"bytes {start}-{endByte}/{mdata.byteSize}"
                |>.stream (sendFileStream handle len)
      | none =>
          let handle ← IO.FS.Handle.mk file.path .read
          let ranges := req.line.headers.get? .range |>.bind (parseRange ·.value)
          let baseResp := Response.ok
            |>.header .contentType (MimeType.mimeType file.path)
            |>.header .acceptRanges headerBytes
          match pickRange ranges fileSize with
          | none =>
            baseResp |>.stream (sendFileStream handle fileSize)
          | some (start, len) =>
            if start >= fileSize then
              Response.new.status .rangeNotSatisfiable
                |>.header! "content-range" s!"bytes */{mdata.byteSize}"
                |>.empty
            else
              skipBytes handle start
              let endByte := start + len - 1
              baseResp
                |>.status .partialContent
                |>.header! "content-range" s!"bytes {start}-{endByte}/{mdata.byteSize}"
                |>.stream (sendFileStream handle len)

end LeanIO
