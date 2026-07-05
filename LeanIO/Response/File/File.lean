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

instance : IntoResponse File where
  into_response f := do
    let file ← f
    if !(←file.path.pathExists) || (←file.path.isDir) then
      Response.notFound |>.empty
    else
      let mdata ← file.path.metadata
      let fileSize := mdata.byteSize.toNat
      let handle ← IO.FS.Handle.mk file.path .read
      Response.ok
          |>.header .contentType (MimeType.mimeType file.path)
          |>.stream (sendFileStream handle fileSize)

end LeanIO
