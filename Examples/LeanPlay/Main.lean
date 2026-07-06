import LeanIO
open Std Async
open Std Http Server
open LeanIO
open LeanIO.Router
open LeanIO.Middlewares
open Lean
open Std

structure MediaEntry where
  name : String
  size : UInt64
  url  : String
deriving ToJson

structure Comment where
  id     : Nat
  video  : String
  author : String
  text   : String
deriving ToJson, FromJson

structure CreateComment where
  author : String
  text   : String
deriving FromJson

structure CommentStore where
  ref : IO.Ref (Array Comment)
deriving TypeName

private def commentMiddleware := do
  let ref ← IO.mkRef (#[] : Array Comment)
  return withExtension CommentStore { ref }

instance : FromRequestParts CommentStore where
  from_request_parts req :=
    match req.extensions.get CommentStore with
    | some s => .ok s
    | none => .error "comment store not installed"

private def staticDir : System.FilePath := "Examples/LeanPlay/static"

def listVideos := GET "/videos" => do
  let entries : Array IO.FS.DirEntry ← System.FilePath.readDir (staticDir / "media")
  let mut videos := #[]
  for entry in entries do
    let path := entry.path
    let mdata ← path.metadata
    let url := s!"/media/{entry.fileName}"
    videos := videos.push { name := entry.fileName, size := mdata.byteSize, url : MediaEntry }
  return videos

def getComments := GET "/videos/{name}/comments" (⟨name⟩ : Path String) (⟨ref⟩ : CommentStore) => do
  let cs : Array Comment ← ref.get
  return cs.filter (fun (c : Comment) => c.video == name)

def postComment := POST "/videos/{name}/comments"
    (⟨body⟩ : Json CreateComment) (⟨name⟩ : Path String) (⟨ref⟩ : CommentStore) => do
  let cs : Array Comment ← ref.get
  let id := cs.size + 1
  let comment : Comment := { id, video := name, author := body.author, text := body.text }
  ref.set (cs.push comment)
  return (Status.created, comment)

def index := GET "/" => do
  return { path := staticDir / "index.html" : File }

def serveStatic := GET "/{*rest}" (⟨_⟩ : Path String) (p : URI.Path) => do
  let decoded := String.intercalate "/" (p.toDecodedSegments.toList)
  return { path := staticDir / decoded : RangeFile }

def main : IO Unit := Async.block do
  let apiRouter : Router := Router.empty
    |>.addRoute listVideos
    |>.addRoute getComments
    |>.addRoute postComment
    |>.addMiddleware catchErrors

  let router : Router := Router.empty
    |>.addRouter "/api/v1" apiRouter
    |>.addRoute index
    |>.addRoute serveStatic
    |>.addMiddleware (← commentMiddleware)
    |>.addMiddleware requestLogger
    |>.addMiddleware catchErrors

  let addr : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, 8080⟩
  let server ← Server.serve addr router
  IO.println "LeanPlay running at http://127.0.0.1:8080"
  server.waitShutdown
