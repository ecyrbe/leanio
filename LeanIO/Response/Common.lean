module

public import Std.Http.Data.Body.Stream
public import LeanIO.Data.Headers.HeaderName

namespace LeanIO
open Std.Http Std.Async

/-- Check whether the request's `If-None-Match` matches the given ETag. -/
public def etagMatches (req : Request Body.Stream) (etag : Header.Value) : Bool :=
  match req.line.headers.get? .ifNoneMatch with
  | some reqEtag => reqEtag == etag
  | none => false

end LeanIO
