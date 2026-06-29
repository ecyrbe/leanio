import LeanIO.Router
import LeanIO.Middlewares
import LeanIO.Request.FromRequestParts
import LeanIO.Request.FromRequestBody
import LeanIO.Response.IntoResponse
open Std Async
open Std Http Server
open LeanIO
open LeanIO.Router
open LeanIO.Middlewares
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
-- State helpers
-- ==========================================

def todoMiddleware := do
  let todoRef ← IO.mkRef { todos := ∅, comments := ∅, nextTodoId := 1, nextCommentId := 1 : TodoStore }
  let todoWrapper := { ref := todoRef : TodoStoreRef }
  return withExtension TodoStoreRef todoWrapper

instance : FromRequestParts TodoStoreRef where
  from_request_parts req :=
    match req.extensions.get TodoStoreRef with
    | some ref => .ok ref
    | none => .error "todo state middleware not installed"

-- ==========================================
-- Todo Routes
-- ==========================================
def test: (TodoStoreRef → ContextAsync (Array Todo)) := fun (⟨ref⟩ : TodoStoreRef) => do
    let store ← ref.get
    return store.todos.valuesArray

def listTodos := GET "/todos" (⟨ref⟩ : TodoStoreRef) => do
    let store ← ref.get
    return store.todos.valuesArray

def getTodoById := GET "/todos/{id}" (⟨ref⟩ : TodoStoreRef) (⟨id⟩ : Path Nat) => do
    match (← ref.get).todos.get? id with
    | some todo => return Except.ok todo
    | none      => return Except.error (Status.notFound, APIError.mk "Not Found" s!"Todo {id} not found")

def createTodo := POST "/todos" (⟨body⟩ : Json CreateTodoRequest) (⟨ref⟩ : TodoStoreRef) => do
    let store ← ref.get
    let newId := store.nextTodoId
    let todo := Todo.ofCreateTodo body newId false
    ref.set { store with todos := store.todos.insert newId todo, nextTodoId := newId + 1 }
    return (Status.created, todo)

def updateTodo := PUT "/todos/{id}" (⟨body⟩ : Json UpdateTodoRequest) (⟨ref⟩ : TodoStoreRef) (⟨id⟩ : Path Nat) => do
    let store ← ref.get
    match store.todos.get? id with
    | none => return Except.error (Status.notFound, APIError.mk "Not Found" s!"Todo {id} not found")
    | some todo =>
      let updated := todo.ofUpdateTodo body
      ref.set { store with todos := store.todos.insert id updated }
      return Except.ok updated

def deleteTodo := DELETE "/todos/{id}" (⟨ref⟩ : TodoStoreRef) (⟨id⟩ : Path Nat) => do
    let store ← ref.get
    match store.todos.get? id with
    | some _ =>
      ref.set { store with todos := store.todos.erase id }
      return Except.ok s!"Todo {id} deleted"
    | none => return Except.error (Status.notFound, APIError.mk "Not Found" s!"Todo {id} not found")

def throwTest := GET "/error" => do
    throw <| IO.userError "middleware test exception"

-- ==========================================
-- Comment Routes
-- ==========================================

def listComments := GET "/todos/{id}/comments" (⟨ref⟩ : TodoStoreRef) (⟨id⟩ : Path Nat) => do
    let store ← ref.get
    let comments := store.comments.fold (init := []) fun acc (_ : Nat) (c : Comment) => if c.todoId == id then c :: acc else acc
    return comments

def getComment := GET "/todos/{id}/comments/{cId}" (⟨ref⟩ : TodoStoreRef) (⟨id, cId⟩ : Path (Nat × Nat)) => do
    match (← ref.get).comments.get? cId with
    | some c => if c.todoId == id then return Except.ok c else return Except.error (Status.notFound, APIError.mk "Not Found" s!"Comment {cId} not found for Todo {id}")
    | none => return Except.error (Status.notFound, APIError.mk "Not Found" s!"Comment {cId} not found")

def createComment := POST "/todos/{id}/comments" (⟨body⟩ : Json CreateCommentRequest) (⟨ref⟩ : TodoStoreRef) (⟨id⟩ : Path Nat) => do
    let store ← ref.get
    let newId := store.nextCommentId
    let comment := Comment.ofCreate body newId id
    ref.set { store with
      comments := store.comments.insert newId comment
      nextCommentId := newId + 1 }
    return (Status.created, comment)

def updateComment := PUT "/todos/{id}/comments/{cId}" (⟨body⟩ : Json UpdateCommentRequest) (⟨ref⟩ : TodoStoreRef) (⟨id, cId⟩ : Path (Nat × Nat)) => do
    let store ← ref.get
    match store.comments.get? cId with
    | none => return Except.error (Status.notFound, APIError.mk "Not Found" s!"Comment {cId} not found")
    | some c =>
      if c.todoId ≠ id then
        return Except.error (Status.notFound, APIError.mk "Not Found" s!"Comment {cId} not found for Todo {id}")
      else
        let updated := c.ofUpdate body
        ref.set { store with comments := store.comments.insert cId updated }
        return Except.ok updated

def deleteComment := DELETE "/todos/{id}/comments/{cId}" (⟨ref⟩ : TodoStoreRef) (⟨id, cId⟩ : Path (Nat × Nat)) => do
    let store ← ref.get
    match store.comments.get? cId with
    | some c =>
      if c.todoId ≠ id then
        return Except.error (Status.notFound, APIError.mk "Not Found" s!"Comment {cId} not found for Todo {id}")
      ref.set { store with comments := store.comments.erase cId }
      return Except.ok s!"Comment {cId} deleted"
    | none => return Except.error (Status.notFound, APIError.mk "Not Found" s!"Comment {cId} not found")

def anyRoute := GET "/{*rest}" (⟨rest⟩ : Path String) =>
    pure (Status.notFound, APIError.mk "Not Found" s!"no matching route for '{rest}'")

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
