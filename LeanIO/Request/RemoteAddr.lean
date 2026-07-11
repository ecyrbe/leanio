module

public import LeanIO.Request.FromRequestParts
public import Std.Net.Addr

namespace LeanIO

open Std.Http Std.Net

public instance : FromRequestParts Std.Http.Server.RemoteAddr where
  from_request_parts req :=
    match req.extensions.get Std.Http.Server.RemoteAddr with
    | some ra => .ok ra
    | none => .error (.io_error "remote address not available — is Std.Http.Server in use?")

end LeanIO
