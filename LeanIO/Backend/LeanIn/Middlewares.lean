module

public import LeanIO.Backend.LeanIn
public import LeanIO.Utils
public import LeanIO.Data.Redacted

/-!
# Middleware for the native backend

The same four middleware LeanIO ships, written against the native handler
(`Request → ResponseWriter → VM Unit`) instead of `HandlerFn`/`ContextAsync`.
Names, parameters and composition order are unchanged; an application switches
the import from `LeanIO.Middlewares` to `LeanIO.Backend.LeanIn.Middlewares`.
-/

namespace LeanIO.Backend.LeanIn.Middlewares

open LeanIn Std.Http
open LeanIO (Redacted)
open LeanIO.Utils
open Std.Http.Header.Value

/-- Error scope: the backend's existing middleware (ordinary errors become a 500; transport cancellation
passes through). -/
public def catchErrors : Middleware := LeanIO.Backend.LeanIn.catchErrors

/-- A response through the writer. -/
public def respond (w : LeanIn.Http.Server.ResponseWriter) (status : Nat) (reason : String)
    (body : ByteArray) (headers : Array (String × String) := #[]) : LeanIn.Runtime.VM Unit := do
  w.start { status, reason, headers := headers.push ("Content-Length", toString body.size) }
  w.write body
  w.finish

/-- Log method, path and response status, as `LeanIO.Middlewares.requestLogger`. -/
public def requestLogger : Middleware := fun next req w => do
  let start ← (monadLift (IO.monoNanosNow) : LeanIn.Runtime.VM _)
  let method := req.head.methodTokenString
  monadLift ((IO.println s!"→ {method} {req.path}" : IO Unit))
  next req w
  let code ← w.code
  let stop ← (monadLift (IO.monoNanosNow) : LeanIn.Runtime.VM _)
  monadLift ((IO.println s!"← {code.getD 0} ({(stop - start) / 1000}us)" : IO Unit))
  monadLift ((IO.getStdout >>= fun s => s.flush) : IO Unit)

/-- Insert a typed value into every request's extensions, as `LeanIO.Middlewares.withExtension`. -/
public def withExtension (α : Type) [TypeName α] (data : α) : Middleware := fun next req w =>
  next { req with extensions := req.extensions.insert data } w

/-- The same challenge values as `LeanIO.Middlewares.Auth`. -/
public def basicUnauthorized : Header.Value := .mk "Basic realm=\"Restricted Area\""
public def bearerUnauthorized : Header.Value := .mk "Bearer realm=\"Restricted Area\""

/-- The `Authorization` value, if present. -/
public def extractAuthorization (req : Request) : Option String :=
  req.header? "authorization"

/-- The authentication validator runs in `VM`, the backend's effect. -/
public inductive AuthConfig where
  | basic (validate : String → Redacted → LeanIn.Runtime.VM Bool)
  | bearer (validate : Redacted → LeanIn.Runtime.VM Bool)

/-- Basic/bearer authentication, with the same responses as `LeanIO.Middlewares.auth`. -/
public def auth (config : AuthConfig) : Middleware := fun next req w => do
  let unauthorized (challenge : Header.Value) :=
    respond w 401 "Unauthorized" .empty #[("WWW-Authenticate", toString challenge)]
  let some headerAuth := extractAuthorization req | unauthorized (match config with
    | .basic _ => basicUnauthorized
    | .bearer _ => bearerUnauthorized)
  match config with
  | .basic validate =>
    match parseBasicAuth headerAuth with
    | some (user, pass) =>
      if ← validate user pass then next req w
      else respond w 403 "Forbidden" .empty
    | none => unauthorized basicUnauthorized
  | .bearer validate =>
    match parseBearer headerAuth with
    | some token =>
      if ← validate token then next req w
      else respond w 403 "Forbidden" .empty
    | none => unauthorized bearerUnauthorized

end LeanIO.Backend.LeanIn.Middlewares
