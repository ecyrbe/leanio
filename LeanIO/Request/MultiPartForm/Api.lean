import Std.Async.ContextAsync
import Std.Data.ByteSlice
import LeanIO.Request.FromRequestBody
import LeanIO.Data.MimeType
import LeanIO.Request.MultiPartForm.Defs
import LeanIO.Request.MultiPartForm.Stream
import LeanIO.Request.MultiPartForm.Parse

namespace LeanIO
open Std.Http Std.Async Std.Slice MimeType

def MultiPartForm.nextEntry (mp : MultiPartForm) : ContextAsync (Option MultipartEntry) := do
  let st ← mp.inner.get
  match st.phase with
  | .done => return none
  | .inFile => return none
  | .ready => parseNextEntry mp.inner

def FormFile.stream (f : FormFile) (cb : ByteArray → ContextAsync Unit) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile => startStreamFile f cb
  | _ => return

def FormFile.discard (f : FormFile) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile =>
    f.inner.modify fun s => { s with cb := ChunkBuffer.empty, pos := 0 }
    streamRaw f.inner st.boundSepSearch (fun _ => pure ()) ChunkBuffer.empty
  | _ => return

def FormFile.save (f : FormFile) (path : System.FilePath) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile =>
    let handle ← IO.FS.Handle.mk path .write
    let fStream := IO.FS.Stream.ofHandle handle
    f.stream fun chunk => do fStream.write chunk
    fStream.flush
  | _ => return

instance : HasMimeTypes (MultiPartForm) where
  mimes? := some [MimeType.multipartForm]

instance : FromRequestBody MultiPartForm where
  from_request_body req := do
    let ctOpt := req.line.headers.get? .contentType
    let ctStr : String :=
      match ctOpt with
      | none => ""
      | some hv => toString hv
    if !ctStr.startsWith "multipart/form-data" then
      return .error s!"multipart/form-data content-type expected, received: {ctStr}"
    let some boundary := extractBoundary ctStr
      | return .error "failed to extract boundary from content-type"
    let boundSep := ("\r\n--" ++ boundary).toUTF8
    let innerVal : MultipartInner := {
        boundStart := boundSep.extract 2 boundSep.size
        boundSepSearch := Search.new (ChunkBuffer.ofByteArray boundSep)
        stream := req.body
      }
    let ref ← IO.mkRef innerVal
    return .ok { inner := ref }

end LeanIO
