module

public import Std.Http.Data.Body.Stream

namespace LeanIO
open Std.Http Std.Async

def chunkSize : Nat := 8192

/-- Compute a weak ETag from file metadata (mtime + size). -/
public def computeETag (mdata : IO.FS.Metadata) : Header.Value :=
  Header.Value.ofString! s!"\"{mdata.modified.sec}{mdata.modified.nsec}{mdata.byteSize}\""

/-- Skip `n` bytes from a file handle. -/
public def skipBytes (handle : IO.FS.Handle) (n : Nat) : IO Unit := do
  let mut skipped := 0
  while skipped < n do
    let bytes ← handle.read (USize.ofNat (min chunkSize (n - skipped)))
    if bytes.isEmpty then break
    skipped := skipped + bytes.size

public def sendFileStream (handle : IO.FS.Handle) (knownLen : Nat) (stream : Body.Stream) : Async Unit := do
  try
    let s := IO.FS.Stream.ofHandle handle
    stream.setKnownSize (some (.fixed knownLen))
    let mut remaining := knownLen
    while remaining > 0 do
      let n := min chunkSize remaining
      let bytes ← s.read (USize.ofNat n)
      if bytes.isEmpty then break
      stream.send { data := bytes }
      remaining := remaining - bytes.size
  finally
    stream.close

/--
Whether `rest` can be joined onto a root directory without escaping it.

Rejects absolute fragments, which discard the root entirely (`root / "/etc/passwd"`
is `/etc/passwd`, reachable from one encoded slash), and `..` components, which
neither `/` nor `FilePath.normalize` resolves. Symlinks inside the root are
followed, matching `tower-http`'s `ServeDir`.
-/
public def isContainableFragment (rest : System.FilePath) : Bool :=
  !rest.isAbsolute && !(rest.components.any (· == ".."))

/-- `rest` joined beneath `root`, or `none` if it would escape. -/
public def resolveUnder (root : System.FilePath) (rest : System.FilePath) :
    Option System.FilePath :=
  if isContainableFragment rest then some (root / rest) else none

/--
The path a `File`/`RangeFile` should open, or `none` if it must 404. Without a
`root` there is no boundary to check against, so the rootless form refuses `..`
but cannot tell a deliberate absolute `path` from an injected one.
-/
public def resolveServePath (root : Option System.FilePath) (path : System.FilePath) :
    Option System.FilePath :=
  match root with
  | some r => resolveUnder r path
  | none => if path.components.any (· == "..") then none else some path

end LeanIO
