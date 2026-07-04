import Lean
import Std.Async.ContextAsync
import Std.Data.ByteSlice
import LeanIO.Utils
import LeanIO.Data.ChunkBuffer

namespace LeanIO
open Std.Http Std.Async Std.Slice

/--
Parser lifecycle state for multipart form processing.
-/
inductive Phase : Type where
  | ready
  | inFile
  | done

instance : Inhabited Phase where
  default := .ready

/--
Internal mutable state shared across the multipart parsing pipeline.

Tracks the buffered chunk data, cursor position, precompiled boundary
search patterns, and the underlying HTTP body stream.
-/
structure MultipartInner : Type where

  /--
  Buffered chunk data that accumulates socket reads.
  -/
  cb             : ChunkBuffer := ChunkBuffer.empty

  /--
  Cursor position within `cb` marking the next byte to process.
  -/
  pos            : Nat := 0

  /--
  The first boundary delimiter (`--boundary` without leading CRLF).

  Used to skip the preamble section before the first part.
  -/
  boundStart     : ByteArray

  /--
  Precompiled KMP search object for `\r\n--boundary`.

  This is the separator between consecutive parts. Finding it marks
  the end of the current part body.
  -/
  boundSepSearch : Search ChunkBuffer

  /--
  The underlying HTTP request body stream.
  -/
  stream         : Body.Stream

  /--
  Current parser lifecycle state (`ready`, `inFile`, or `done`).
  -/
  phase          : Phase := .ready

  /--
  Monotonic counter used to disambiguate auto-generated filenames

  for parts that have no `filename` parameter in Content-Disposition.
  -/
  nameCounter    : Nat := 0

/--
A file part within a multipart form submission.

Contains parsed Content-Disposition metadata, the full set of part
headers, and a reference to the shared parser state so the body can
be streamed lazily via `stream`, `save`, `bytes`, or `discard`.

## Reading the body

| Method | Description |
|---|---|
| `f.save path` | Stream chunks to disk |
| `f.stream cb` | Call `cb` for each chunk (zero-copy) |
| `f.bytes` | Read all chunks into a `ByteArray` (small files only) |
| `f.discard` | Skip the body |

## Metadata fields

- `name` — form field name from Content-Disposition
- `filename` — original filename (or auto-generated when absent and content type is not `text/*`)
- `contentType` — Content-Type (defaults to `text/plain` per RFC 2046 §5.1)
- `headers` — all part headers as a `Std.Http.Headers` map
-/
structure FormFile where
  name        : String
  filename    : String
  contentType : String
  headers     : Headers
  inner       : IO.Ref MultipartInner

/--
One entry in a multipart form submission: either a simple form field
or an uploaded file.
-/
inductive MultipartEntry : Type where

  /--
  A simple form field with `name` and its string `value`.
  -/
  | field (name : String) (value : String)

  /--
  An uploaded file part containing metadata and a lazily-streamable body.
  -/
  | file  (file : FormFile)

/--
Streaming multipart form parser. Consumes request body lazily — file
contents are never fully buffered.

## Route declaration

```
open LeanIO

def router : Router :=
  POST "/upload" (mp : MultiPartForm) => do
    while let some entry := ← mp.nextEntry do
      match entry with
      | .field name value => IO.println s!"field {name} = {value}"
      | .file file =>
        file.save s!"uploads/{file.filename}"
    return Status.ok
```

## Per-part handling

| Entry variant | When | What to do |
|---|---|---|
| `.field name value` | Simple form field | `value` is already buffered |
| `.file file` | File upload | `name`, `filename`, `contentType`, `headers` available immediately; body is streamed |

## Reading file bodies

| Method | Description |
|---|---|
| `file.save path` | Stream chunks to disk |
| `file.stream cb` | Call `cb` for each chunk (zero-copy) |
| `file.bytes` | Read all chunks into a `ByteArray` (small files only) |
| `file.discard` | Skip the body |
-/
structure MultiPartForm where

  /--
  Shared mutable parser state.
  -/
  inner : IO.Ref MultipartInner

end LeanIO
