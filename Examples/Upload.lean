import LeanIO
open Std Async
open Std Http Server
open LeanIO
open LeanIO.Router
open LeanIO.Middlewares
open Lean

set_option linter.unusedVariables false

def uploadsDir : System.FilePath := "uploads"

structure LoginForm where
  username : String
  password : String
  role     : String := "user"
deriving FromForm

structure UploadResult where
  filename : String
  size     : Nat
  path     : String
  deriving ToJson

structure UploadResponse where
  fields : List (String × String)
  files  : List UploadResult
  deriving ToJson

def upload := POST "/upload" (mp : MultiPartForm) => do
  let mut fields : List (String × String) := []
  let mut files : List UploadResult := []
  IO.FS.createDirAll uploadsDir
  while let some entry := ← mp.nextEntry do
    match entry with
    | .field name value =>
      fields := (name, value) :: fields
    | .file file =>
      let savePath := uploadsDir / file.filename
      file.save savePath
      let mdata ← System.FilePath.metadata savePath
      files := { filename := file.filename, size := mdata.byteSize.toNat, path := toString savePath } :: files
  return { fields := fields.reverse, files := files.reverse : UploadResponse }

def login := POST "/login" (⟨form⟩ : Form LoginForm) => do
  return s!"logged in as {form.username} ({form.role})"

def main : IO Unit := Async.block do
  let addr : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, 8081⟩
  let router := Router.empty
    |>.addRoute upload
    |>.addRoute login
    |>.addMiddleware catchErrors
    |>.addMiddleware requestLogger
  let server ← Server.serve addr router { maxBodySize := 512 * 1024 * 1024 : Config }
  IO.println "Upload server on http://127.0.0.1:8081"
  server.waitShutdown
