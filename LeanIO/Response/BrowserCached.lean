module

import LeanIO.Data.Headers.CacheControl
import LeanIO.Response.Common
import LeanIO.Response.IntoResponse
import Std.Http

namespace LeanIO
open Std.Http Lean

/--
  A response that will be cached by the browser and the CDN.
  The only purpose of it is to save bandwidth for the clients.
  The handler is still called, and etag is computed on each handler handler anwser
  ETag is computed with String.hash
  You can customize the cache control by setting `cacheControl`
  by default cache control is set to `"private, no-cache"`
-/
structure BrowserCached (α : Type) where
  new ::
  value: α
  cacheControl: CacheControl := .userPrivate

instance [ToJson α] : IntoResponseExt (BrowserCached α) where
  into_response_ext req cached := do
    let cached ← cached
    let jsonStr := Json.pretty <| toJson cached.value
    let etag := Header.Value.ofString! s!"\"{jsonStr.hash}\""
    if etagMatches req etag then
      Response.new |>.status Status.notModified |>.empty
    else
      Response.ok
        |>.header .etag etag
        |>.header .cacheControl cached.cacheControl
        |>.json jsonStr

end LeanIO
