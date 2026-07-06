import Std.Async.ContextAsync
import LeanIO.Response.IntoResponse
import LeanIO.Request.HeaderRange
import LeanIO.Data.HeaderName
import LeanIO.Data.MimeType
import LeanIO.Response.File.Utils

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
  path   : System.FilePath

instance : IntoResponseExt File where
  into_response_ext req f := do
    let file ← f
    if !(←file.path.pathExists) || (←file.path.isDir) then
      Response.notFound |>.empty
    else
      let mdata ← file.path.metadata
      let etag := computeETag mdata
      let fileSize := mdata.byteSize.toNat
      if etagMatches req etag then
        Response.new |>.status Status.notModified |>.empty
      else
        let handle ← IO.FS.Handle.mk file.path .read
        Response.ok
          |>.header .contentType (MimeType.mimeType file.path)
          |>.header .cacheControl (Header.Value.mk "public, max-age=0, must-revalidate")
          |>.header .etag etag
          |>.stream (sendFileStream handle fileSize)

end LeanIO
