import LeanIO
open Std Async
open Std Http Server
open LeanIO
open LeanIO.Router
open LeanIO.Middlewares

private def staticDir : System.FilePath := "Examples/WhoAmI/static"

def whoami := GET "/api/whoami" (⟨addr⟩ : RemoteAddr) => do
    return toString addr

def index := GET "/" => do
    return { path := staticDir / "index.html"
             cacheControl := CacheControl.disabled : File }

def serveStatic := GET "/{*rest}" (⟨rest⟩ : Path String) => do
    return { path := staticDir / rest : File }

def main : IO Unit := Async.block do
  let router : Router := Router.empty
    |>.addRoute whoami
    |>.addRoute index
    |>.addRoute serveStatic
    |>.addMiddleware catchErrors
    |>.addMiddleware requestLogger

  let addr : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, 8080⟩
  let server ← router.serve addr
  IO.println "WhoAmI running at http://127.0.0.1:8080"
  server.waitShutdown
