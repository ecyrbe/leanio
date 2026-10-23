module

public import Std.Async.ContextAsync
import Std.Data.ByteSlice
public import LeanIO.Request.FromRequestBody
public import LeanIO.Data.Headers.MimeType
public import LeanIO.Request.MultiPartForm.Defs
public import LeanIO.Request.MultiPartForm.Headers
public import LeanIO.Request.MultiPartForm.Stream
public import LeanIO.Request.MultiPartForm.Parse

namespace LeanIO
open Std.Http Std.Async Std.Slice MimeType

variable {m : Type → Type} {body : Type} [Monad m] [MonadLift IO m] [BodySource m body]

/--
Return the next entry (field or file) from the multipart stream,
or `none` when all parts have been consumed.

Must be called repeatedly until `none` is returned. After a file
entry, the file body must be fully consumed (via `stream`, `save`,
`bytes`, or `discard`) before calling `nextEntry` again.
-/
public def MultiPartForm.nextEntry [Inhabited (m Unit)] [Inhabited (m (ChunkBuffer × Nat))]
    (mp : MultiPartForm m body) : m (Option (MultipartEntry m body)) := do
  let st ← refGet mp.inner
  match st.phase with
  | .done => return none
  | .inFile => return none
  | .ready => parseNextEntry mp.inner

/--
Stream the file body chunk by chunk via a callback.

Each chunk is passed directly from the underlying `Body.Stream`
without accumulation — memory usage is bounded by the chunk size.

## Example

```
file.stream fun chunk => do
  IO.FS.writeBinFile "output.bin" chunk
```
-/
public def FormFile.stream [Inhabited (m (ChunkBuffer × Nat))]
    (f : FormFile m body) (cb : ByteArray → m Unit) : m Unit := do
  let st ← refGet f.inner
  match st.phase with
  | .inFile => startStreamFile f cb
  | _ => return

/--
Read and discard the file body without storing it.

Useful for skipping unused file parts — body is consumed chunk
by chunk, never buffered.
-/
public def FormFile.discard [Inhabited (m (ChunkBuffer × Nat))]
    (f : FormFile m body) : m Unit := do
  let st ← refGet f.inner
  match st.phase with
  | .inFile =>
    refModify f.inner fun s => { s with cb := ChunkBuffer.empty, pos := 0 }
    streamRaw f.inner st.boundSepSearch (fun _ => pure ()) ChunkBuffer.empty
  | _ => return

/--
Stream the file body to disk at the given path.

Chunks are written directly to the file handle — the body is never
fully buffered in memory. The file is created if it does not exist.

## Example

```
file.save <| dir / file.filename
```
-/
public def FormFile.save [Inhabited (m (ChunkBuffer × Nat))]
    (f : FormFile m body) (path : System.FilePath) : m Unit := do
  let st ← refGet f.inner
  match st.phase with
  | .inFile =>
    let handle ← (monadLift (IO.FS.Handle.mk path .write) : m _)
    let fStream := IO.FS.Stream.ofHandle handle
    f.stream fun chunk => monadLift (fStream.write chunk : IO Unit)
    monadLift (fStream.flush : IO Unit)
  | _ => return

public instance : HasMimeTypes (MultiPartForm ContextAsync Body.Stream) where
  mimes? := some [MimeType.multipartForm]

/--
`FromRequestBody` instance that constructs a `MultiPartForm` parser
from a request with Content-Type `multipart/form-data`.

Extracts the `boundary` parameter and precompiles KMP search patterns
for boundary detection.
-/
public instance : FromRequestBody (MultiPartForm ContextAsync Body.Stream) where
  from_request_body req := do
    match checkMimeTypes MultiPartForm req.line.headers with
    | .error e => return .error (.bad_media_error e)
    | .ok _ =>
      let some contentType := req.line.headers.get? .contentType | return .error (.bad_media_error "missing Content-Type header")
      let some boundary := extractParam (toString contentType) "boundary" | return .error (.syntax_error "failed to extract boundary from content-type")
      let boundSep := ("\r\n--" ++ boundary).toUTF8
      let inner : MultipartInner ContextAsync Body.Stream := {
          boundStart := boundSep.extract 2 boundSep.size
          boundSepSearch := Search.new (ChunkBuffer.ofByteArray boundSep)
          stream := req.body
        }
      let ref ← IO.mkRef inner
      return .ok { inner := ref }

end LeanIO
