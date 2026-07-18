import LeanIO
open Std Async
open Std Http Server
open LeanIO
open LeanIO.Router
open LeanIO.Middlewares
open Lean

structure CreateUser where
  name  : String
  email : String
deriving FromJson, FromForm, ToJson

def createUser := POST "/users"
    (body : Json CreateUser ⊕ Form CreateUser) => do
  let data : CreateUser := match body with
    | Sum.inl j => j.body
    | Sum.inr f => f.value
  return (Status.created, data)

def main : IO Unit := Async.block do
  let router := Router.empty
    |>.addRoute createUser
    |>.addMiddleware catchErrors
    |>.addMiddleware requestLogger
  let addr : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, 8089⟩
  let server ← router.serve addr
  IO.println "Sum test on http://127.0.0.1:8089"
  server.waitShutdown
