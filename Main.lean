import Leanio.Router
open Std Async
open Std Http Server
open Leanio.Router

set_option linter.unusedVariables false

GET "/hello" hello (req : Request Body.Stream) :=
  Response.ok |>.text "Hello, World!"

GET "/user/{id}" userById (req : Request Body.Stream) (id : Nat) :=
  Response.ok |>.text s!"User {id}"

GET "/posts/{year}/{month}" postByDate (req : Request Body.Stream) (year : Int) (month : Int) :=
  Response.ok |>.text s!"Posts from {year}-{month}"

POST "/user" createUser (req : Request Body.Stream) :=
  Response.created |>.text "User created"

DELETE "/user/{id}" deleteUser (req : Request Body.Stream) (id : Nat) :=
  Response.ok |>.text s!"User {id} deleted"

-- Nested API sub-router
GET "/status" apiStatus (req : Request Body.Stream) :=
  Response.ok |>.text "API is healthy"

GET "/items" listItems (req : Request Body.Stream) :=
  Response.ok |>.text "Item list"

GET "/items/{id}" getItem (req : Request Body.Stream) (id : Nat) :=
  Response.ok |>.text s!"Item {id}"

def apiRouter : Router :=
  { routes := [apiStatus, listItems, getItem] }

def myRouter : Router :=
  { routes := [hello, userById, postByDate, createUser, deleteUser]
  , routers := [("/api", apiRouter)]
  , middlewares := [loggingMiddleware]
  }

def main : IO Unit := Async.block do
  let addr : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, 8080⟩
  let server ← Server.serve addr myRouter
  IO.println "Listening on http://127.0.0.1:8080"
  server.waitShutdown
