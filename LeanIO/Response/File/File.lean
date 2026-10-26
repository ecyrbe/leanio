module

import Std.Async.ContextAsync
public import LeanIO.Response.Common
public import LeanIO.Response.IntoResponse
import LeanIO.Request.HeaderRange
public import LeanIO.Data.Headers.HeaderName
public import LeanIO.Data.Headers.MimeType
public import LeanIO.Response.File.Utils
public import LeanIO.Data.Headers.CacheControl


namespace LeanIO
open Std.Http Std.Async

/--
Serve a file on disk using streaming.

`IntoResponse` is implemented with streaming and `Content-Length` framing
(not chunked transfer encoding).

Use `File.under` for request input; joining it onto a directory by hand potentially
introduces a security vulnerability.

```lean
def serveFile := GET "/static/{*rest}" (⟨rest⟩ : Path String) => do
  return File.under "static" rest
```
-/
public structure File where
  private mk ::
  path         : System.FilePath
  /-- Directory `path` must stay inside, when it came from a client. -/
  root         : Option System.FilePath
  cacheControl : Option CacheControl
deriving Inhabited

/-- A `File` serving `rest` from inside `root`. Use this for request input. -/
public def File.under (root rest : System.FilePath)
    (cacheControl : Option CacheControl := some <| .publicStatic 0) : File :=
  .mk rest (some root) cacheControl

/--
A `File` serving `path` as given, with no containment check.

Only for a path the program chose itself. Passing request input here reintroduces
the traversal `File.under` exists to prevent.
-/
public def File.trusted (path : System.FilePath)
    (cacheControl : Option CacheControl := some <| .publicStatic 0) : File :=
  .mk path none cacheControl

/-- Replaces the cache directives on an existing `File`, leaving its path unchanged. -/
public def File.withCacheControl (cacheControl : Option CacheControl) (self : File) : File :=
  .mk self.path self.root cacheControl

public instance : IntoResponseExt File where
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
          Response.ok
            |>.header .contentType (MimeType.mimeType path)
            |>.header .etag etag
            |>.header .cacheControl cacheControl
            |>.stream (sendFileStream handle fileSize)
      | none =>
        let handle ← IO.FS.Handle.mk path .read
        Response.ok
          |>.header .contentType (MimeType.mimeType path)
          |>.stream (sendFileStream handle fileSize)

end LeanIO
