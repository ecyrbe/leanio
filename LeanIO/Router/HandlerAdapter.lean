import Std.Async.ContextAsync
import LeanIO.Router.Route
import LeanIO.Request.FromRequestParts
import LeanIO.Request.FromRequestBody
import LeanIO.Response.IntoResponse

namespace LeanIO
open Std.Http Std.Async

class HandlerAdapter (Fn : Type) where
  adapt : Fn → Router.HandlerSig

private class PartsAdapter (Fn : Type) where
  adaptParts : Fn → Router.HandlerSig

-- HandlerAdapter: root level. Allows 0 or 1 body (first position only).

instance [IntoResponse R] : HandlerAdapter (ContextAsync R) where
  adapt handler _ := IntoResponse.into_response handler

instance [IntoResponse R] : HandlerAdapter (Unit → R) where
  adapt handler _ := IntoResponse.into_response <| pure (handler ())

instance [IntoResponse R] : HandlerAdapter (Unit → ContextAsync R) where
  adapt handler _ := IntoResponse.into_response (handler ())

instance [FromRequestBody P] [PartsAdapter Rest] : HandlerAdapter (P → Rest) where
  adapt handler req := do
    match ← FromRequestBody.from_request_body (α:=P) req with
    | .ok p => PartsAdapter.adaptParts (handler p) req
    | .error e => Response.badRequest |>.text e

instance [FromRequestParts P] [PartsAdapter Rest] : HandlerAdapter (P → Rest) where
  adapt handler req := do
    match FromRequestParts.from_request_parts (α:=P) req with
    | .ok p => PartsAdapter.adaptParts (handler p) req
    | .error e => Response.badRequest |>.text e

-- PartsAdapter: body already consumed. Only FromRequestParts allowed.

private instance [IntoResponse R] : PartsAdapter (ContextAsync R) where
  adaptParts handler _ := IntoResponse.into_response handler

private instance [FromRequestParts P] [PartsAdapter Rest] : PartsAdapter (P → Rest) where
  adaptParts handler req := do
    match FromRequestParts.from_request_parts (α:=P) req with
    | .ok p => PartsAdapter.adaptParts (handler p) req
    | .error e => Response.badRequest |>.text e

end LeanIO
