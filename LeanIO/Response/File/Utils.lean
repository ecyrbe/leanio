module

public import Std.Http.Data.Body.Stream

namespace LeanIO
open Std.Http Std.Async

def chunkSize : Nat := 8192

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
