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

end LeanIO
