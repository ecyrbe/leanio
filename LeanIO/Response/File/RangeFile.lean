module

import Std.Async.ContextAsync
public import LeanIO.Response.Common
public import LeanIO.Response.IntoResponse
public import LeanIO.Response.File.Utils
public import LeanIO.Request.HeaderRange
public import LeanIO.Data.Headers.MimeType
import LeanIO.Data.Headers.HeaderName
public import LeanIO.Data.Headers.CacheControl

namespace LeanIO
open Std.Http Std.Async

public def headerBytes : Header.Value := Header.Value.mk "bytes"

/--
A file on disk served with optional `Range` support for efficient seeking.

returns the appropriate `206 Partial Content` with `Content-Range` for
range requests, or the full file otherwise.

`IntoResponse` is implemented with streaming and `Content-Length` framing
(not chunked transfer encoding).

Use `RangeFile.under` for request input; joining it onto a directory by hand
potentially introduces a security vulnerability.

```lean
def serveFile := GET "/static/{*rest}" (⟨rest⟩ : Path String) => do
  return RangeFile.under "static" rest
```
-/
public structure RangeFile where
  private mk ::
  path         : System.FilePath
  /-- Directory `path` must stay inside, when it came from a client. -/
  root         : Option System.FilePath
  cacheControl : Option CacheControl
deriving Inhabited

/-- A `RangeFile` serving `rest` from inside `root`. Use this for request input. -/
public def RangeFile.under (root rest : System.FilePath)
    (cacheControl : Option CacheControl := some <| .publicStatic 0) : RangeFile :=
  .mk rest (some root) cacheControl

/--
A `RangeFile` serving `path` as given, with no containment check.

Only for a path the program chose itself. Passing request input here reintroduces
the traversal `RangeFile.under` exists to prevent.
-/
public def RangeFile.trusted (path : System.FilePath)
    (cacheControl : Option CacheControl := some <| .publicStatic 0) : RangeFile :=
  .mk path none cacheControl

/-- Replaces the cache directives on an existing `RangeFile`, leaving its path unchanged. -/
public def RangeFile.withCacheControl (cacheControl : Option CacheControl) (self : RangeFile) : RangeFile :=
  .mk self.path self.root cacheControl

public def pickRange (ranges : Option (Array Range)) (fileSize : Nat) : Option (Nat × Nat) :=
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

public instance : IntoResponseExt RangeFile where
  into_response_ext req f := do
    let file ← f
    let some path := resolveServePath file.root file.path
      | Response.notFound |>.empty
    if !(←path.pathExists) || (←path.isDir) then
      Response.notFound |>.empty
    else
      let mdata ← path.metadata
      let fileSize := mdata.byteSize.toNat
      match file.cacheControl with
      | some cacheControl =>
        let etag := computeETag mdata
        if etagMatches req etag then
          Response.new |>.status Status.notModified |>.empty
        else
          let handle ← IO.FS.Handle.mk path .read
          let ranges := req.line.headers.get? .range |>.bind (parseRange ·.value)
          let baseResp := Response.ok
            |>.header .contentType (MimeType.mimeType path)
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
          let handle ← IO.FS.Handle.mk path .read
          let ranges := req.line.headers.get? .range |>.bind (parseRange ·.value)
          let baseResp := Response.ok
            |>.header .contentType (MimeType.mimeType path)
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
