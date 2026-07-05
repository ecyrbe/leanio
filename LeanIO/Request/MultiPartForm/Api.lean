import Std.Async.ContextAsync
import Std.Data.ByteSlice
import LeanIO.Request.FromRequestBody
import LeanIO.Data.MimeType
import LeanIO.Request.MultiPartForm.Defs
import LeanIO.Request.MultiPartForm.Stream
import LeanIO.Request.MultiPartForm.Parse

namespace LeanIO
open Std.Http Std.Async Std.Slice MimeType

/--
Return the next entry (field or file) from the multipart stream,
or `none` when all parts have been consumed.

Must be called repeatedly until `none` is returned. After a file
entry, the file body must be fully consumed (via `stream`, `save`,
`bytes`, or `discard`) before calling `nextEntry` again.
-/
def MultiPartForm.nextEntry (mp : MultiPartForm) : ContextAsync (Option MultipartEntry) := do
  let st ← mp.inner.get
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
def FormFile.stream (f : FormFile) (cb : ByteArray → ContextAsync Unit) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile => startStreamFile f cb
  | _ => return

/--
Read and discard the file body without storing it.

Useful for skipping unused file parts — body is consumed chunk
by chunk, never buffered.
-/
def FormFile.discard (f : FormFile) : ContextAsync Unit := do
  let st ← f.inner.get
  match st.phase with
  | .inFile =>
    f.inner.modify fun s => { s with cb := ChunkBuffer.empty, pos := 0 }
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

/--
`FromRequestBody` instance that constructs a `MultiPartForm` parser
from a request with Content-Type `multipart/form-data`.

Extracts the `boundary` parameter and precompiles KMP search patterns
for boundary detection.
-/
instance : FromRequestBody MultiPartForm where
  from_request_body req := do
    match checkMimeTypes MultiPartForm req.line.headers with
    | .error e => return .error e
    | .ok _ => pure ()
    let some contentType := req.line.headers.get? .contentType | return .error "missing Content-Type header"
    let some boundary := extractParam (toString contentType) "boundary" | return .error "failed to extract boundary from content-type"
    let boundSep := ("\r\n--" ++ boundary).toUTF8
    let inner : MultipartInner := {
        boundStart := boundSep.extract 2 boundSep.size
        boundSepSearch := Search.new (ChunkBuffer.ofByteArray boundSep)
        stream := req.body
      }
    let ref ← IO.mkRef inner
    return .ok { inner := ref }

end LeanIO
