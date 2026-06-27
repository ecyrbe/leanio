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

-- ==========================================
-- Comment structures
-- ==========================================

structure Comment where
  id     : Nat
  todoId : Nat
  text   : String
deriving ToJson

structure CreateCommentRequest where
  text : String
deriving FromJson

structure UpdateCommentRequest where
  text? : Option String := none
deriving FromJson

namespace Comment

def ofCreate (req : CreateCommentRequest) (id todoId : Nat) : Comment :=
  { id, todoId, text := req.text }

def ofUpdate (upd : UpdateCommentRequest) (comment : Comment) : Comment :=
  { comment with text := upd.text?.getD comment.text }

end Comment


structure TodoStore where
  todos          : HashMap Nat Todo
  comments       : HashMap Nat Comment
  nextTodoId     : Nat
  nextCommentId  : Nat

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
  Response.badRequest |>.json <| Json.pretty <|toJson <| APIError.mk "Bad Request" msg

def json.notFound (msg : String) : Async (Response Body.Full) :=
  Response.notFound |>.json <|Json.pretty <|toJson <|APIError.mk "Not Found" msg

def json.internalServerError (msg: String) : Async (Response Body.Full) :=
  Response.internalServerError |>.json <| Json.pretty <| toJson <| APIError.mk "Internal Server Error" msg

end Std.Http.Response

-- ==========================================
-- State helpers
-- ==========================================

def noTodoState : Async (Response Body.Any) :=
  Response.json.internalServerError "todo state middleware not installed"

def withTodoState (req : Request α) (f : IO.Ref TodoStore → Async (Response Body.Any)):=
  match req.extensions.get TodoStoreRef with
  | some wrapper => do
    f wrapper.ref
  | none => noTodoState

def todoMiddleware := do
  let todoRef ← IO.mkRef { todos := ∅, comments := ∅, nextTodoId := 1, nextCommentId := 1 : TodoStore }
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
-- Comment Routes
-- ==========================================

GET "/todos/{id}/comments" listComments (req : Request Body.Stream) (id : Nat) :=
  withTodoState req fun ref => do
    let store ← ref.get
    let comments := store.comments.fold (fun acc _ c => if c.todoId == id then c :: acc else acc) []
    Response.json comments

GET "/todos/{id}/comments/{cId}" getComment (req : Request Body.Stream) (id : Nat) (cId : Nat) :=
  withTodoState req fun ref => do
    let store ← ref.get
    match store.comments.get? cId with
    | some c => if c.todoId == id then Response.json c else Response.json.notFound s!"Comment {cId} not found for Todo {id}"
    | none => Response.json.notFound s!"Comment {cId} not found"

POST "/todos/{id}/comments" createComment (req : Request CreateCommentRequest) (id : Nat) := do
  withTodoState req fun ref => do
    let store ← ref.get
    let newId := store.nextCommentId
    let comment := Comment.ofCreate req.body newId id
    ref.set { store with
      comments := store.comments.insert newId comment
      nextCommentId := newId + 1 }
    Response.json.created comment

PUT "/todos/{id}/comments/{cId}" updateComment (req : Request UpdateCommentRequest) (id : Nat) (cId : Nat) := do
  withTodoState req fun ref => do
    let store ← ref.get
    match store.comments.get? cId with
    | none => Response.json.notFound s!"Comment {cId} not found"
    | some c =>
      if c.todoId ≠ id then
        Response.json.notFound s!"Comment {cId} not found for Todo {id}"
      else
        let updated := c.ofUpdate req.body
        ref.set { store with comments := store.comments.insert cId updated }
        Response.json updated

DELETE "/todos/{id}/comments/{cId}" deleteComment (req : Request Body.Stream) (id : Nat) (cId : Nat) :=
  withTodoState req fun ref => do
    let store ← ref.get
    match store.comments.get? cId with
    | some c =>
      if c.todoId ≠ id then Response.json.notFound s!"Comment {cId} not found for Todo {id}" else
        ref.set { store with comments := store.comments.erase cId }
        Response.ok |>.text s!"Comment {cId} deleted"
    | none => Response.json.notFound s!"Comment {cId} not found"

GET "/{*any}" anyRoute (req: Request Body.Stream) (any : String) :=
  Response.json.notFound s!"No route matches {req.line.uri} / captured = {any}"

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
  |>.addRoute listComments
  |>.addRoute getComment
  |>.addRoute createComment
  |>.addRoute updateComment
  |>.addRoute deleteComment

def rootRouter : Router := Router.empty
  |>.addRouter "/api/v1" todosRouter
  |>.addRoute anyRoute

-- ==========================================
-- Entry point
-- ==========================================

def main : IO Unit := Async.block do
  let addr : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, 8080⟩
  let router := rootRouter
    |>.addMiddleware (← todoMiddleware)
    |>.addMiddleware catchErrors -- add second to last to be sure to catch any error
    |>.addMiddleware requestLogger -- add last to be sure to log all errors
  let server ← Server.serve addr router
  IO.println "Listening on http://127.0.0.1:8080"
  server.waitShutdown
