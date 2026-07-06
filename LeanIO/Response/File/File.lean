import Std.Async.ContextAsync
import LeanIO.Response.IntoResponse
import LeanIO.Request.HeaderRange
import LeanIO.Data.HeaderName
import LeanIO.Data.MimeType
import LeanIO.Response.File.Utils
import LeanIO.Data.CacheControl


namespace LeanIO
open Std.Http Std.Async

/--
Serve a file on disk using streaming.

`IntoResponse` is implemented with streaming and `Content-Length` framing
(not chunked transfer encoding).

```lean
def serveFile := GET "/static/{*rest}" (⟨rest⟩ : Path String) => do
  return { path := "static" / rest : File }
```
-/
structure File where
  new ::
  path         : System.FilePath
  cacheControl : Option CacheControl := some <|.publicStatic 0
deriving Inhabited

instance : IntoResponseExt File where
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
          Response.ok
            |>.header .contentType (MimeType.mimeType file.path)
            |>.header .etag etag
            |>.header .cacheControl cacheControl
            |>.stream (sendFileStream handle fileSize)
      | none =>
        let handle ← IO.FS.Handle.mk file.path .read
        Response.ok
          |>.header .contentType (MimeType.mimeType file.path)
          |>.stream (sendFileStream handle fileSize)

end LeanIO
