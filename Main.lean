import Leanio.Router
import Leanio.Middlewares
open Std Async
open Std Http Server
open Leanio.Router
open Leanio.Middlewares
open Lean
open Std

set_option linter.unusedVariables false

-- ==========================================
-- Data structures
-- ==========================================

structure Todo where
  id        : Nat
  title     : String
  completed : Bool
  userId    : Nat
deriving ToJson

structure CreateTodoRequest where
  title  : String
  userId : Nat
deriving FromJson

structure UpdateTodoRequest where
  title?     : Option String := none
  completed? : Option Bool := none
  userId?    : Option Nat := none
deriving FromJson

namespace Todo

def ofCreateTodo (todo: CreateTodoRequest) (id: Nat) (completed: Bool): Todo :=
 { id, completed, title := todo.title, userId := todo.userId }

def ofUpdateTodo (upd: UpdateTodoRequest) (todo: Todo) : Todo :=
 { todo with
   title     := upd.title?      |>.getD todo.title
   completed  := upd.completed? |>.getD todo.completed
   userId     := upd.userId?    |>.getD todo.userId
 }

end Todo


structure TodoStore where
  todos      : HashMap Nat Todo
  nextTodoId : Nat

structure TodoStoreRef where
  ref : IO.Ref TodoStore
deriving TypeName

structure APIError where
  error   : String
  message : String
deriving ToJson, FromJson

-- ==========================================
-- JSON helpers
-- ==========================================
namespace Std.Http.Response
def json [ToJson α] (j : α) : Async (Response Body.Full) :=
  Response.ok |>.json <| Json.pretty <| toJson j

def json.created [ToJson α] (j : α) : Async (Response Body.Full) :=
  Response.created |>.json <| Json.pretty <| toJson j

def json.badRequest (msg : String) : Async (Response Body.Full) :=
  Response.badRequest |>.json (Json.pretty (toJson (APIError.mk "Bad Request" msg)))

def json.notFound (msg : String) : Async (Response Body.Full) :=
  Response.notFound |>.json (Json.pretty (toJson (APIError.mk "Not Found" msg)))

end Std.Http.Response

-- ==========================================
-- State helpers
-- ==========================================

def noTodoState : Async (Response Body.Any) :=
  Response.internalServerError |>.json (Json.pretty (toJson (APIError.mk "Internal Server Error" "todo state middleware not installed")))

def withTodoState (req : Request α) (f : IO.Ref TodoStore → Async (Response Body.Any)):=
  match req.extensions.get TodoStoreRef with
  | some wrapper => do
    f wrapper.ref
  | none => noTodoState

def todoMiddleware := do
  let todoRef ← IO.mkRef { todos := ∅, nextTodoId := 1 : TodoStore }
  let todoWrapper := { ref := todoRef : TodoStoreRef }
  return withExtension TodoStoreRef todoWrapper

-- ==========================================
-- Todo Routes
-- ==========================================

GET "/todos" listTodos (req : Request Body.Stream) :=
  withTodoState req fun ref => do
    let store ← ref.get
    Response.json store.todos.valuesArray

GET "/todos/{id}" getTodoById (req : Request Body.Stream) (id : Nat) :=
  withTodoState req fun ref => do
    match (←ref.get).todos.get? id with
    | some todo => Response.json todo
    | none      => Response.json.notFound s!"Todo {id} not found"

POST "/todos" createTodo (req : Request CreateTodoRequest) := do
  withTodoState req fun ref => do
    let store ← ref.get
    let newId := store.nextTodoId
    let todo := Todo.ofCreateTodo req.body newId false
    ref.set { store with todos := store.todos.insert newId todo, nextTodoId := newId + 1 }
    Response.json.created todo

PUT "/todos/{id}" updateTodo (req : Request UpdateTodoRequest) (id : Nat) := do
  withTodoState req fun ref => do
    let store ← ref.get
    match store.todos.get? id with
    | none => Response.json.notFound s!"Todo {id} not found"
    | some todo =>
      let updated := todo.ofUpdateTodo req.body
      ref.set { store with todos := store.todos.insert id updated }
      Response.json updated

DELETE "/todos/{id}" deleteTodo (req : Request Body.Stream) (id : Nat) :=
  withTodoState req fun ref => do
    let store ← ref.get
    match store.todos.get? id with
    | some _ =>
      ref.set { store with todos := store.todos.erase id }
      Response.ok |>.text s!"Todo {id} deleted"
    | none => Response.json.notFound s!"Todo {id} not found"

GET "/error" throwTest (req : Request Body.Stream) :=
  throw <| IO.userError "middleware test exception"

-- ==========================================
-- Router construction
-- ==========================================

def todosRouter : Router := Router.empty
  |>.addRoute listTodos
  |>.addRoute getTodoById
  |>.addRoute createTodo
  |>.addRoute updateTodo
  |>.addRoute deleteTodo
  |>.addRoute throwTest

def rootRouter : Router := Router.empty
  |>.addRouter "/api/v1" todosRouter
  |>.addMiddleware requestLogger

-- ==========================================
-- Entry point
-- ==========================================

def main : IO Unit := Async.block do
  let addr : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, 8080⟩
  let router := rootRouter
    |>.addMiddleware (← todoMiddleware)
    |>.addMiddleware catchErrors -- add last to be sure to catch any error
  let server ← Server.serve addr router
  IO.println "Listening on http://127.0.0.1:8080"
  server.waitShutdown
