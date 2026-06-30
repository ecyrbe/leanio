import Std.Async.ContextAsync
import LeanIO.Response.IntoResponse
import LeanIO.Request.HeaderRange

namespace LeanIO
open Std.Http Std.Async

private def headerContentType  : Header.Name := Header.Name.mk "content-type"
private def headerAcceptRanges : Header.Name := Header.Name.mk "accept-ranges"
private def headerContentRange : Header.Name := Header.Name.mk "content-range"

private def mimeOctetStream    : Header.Value := Header.Value.mk "application/octet-stream"
private def mimeTextPlain      : Header.Value := Header.Value.mk "text/plain"
private def mimeTextHtml       : Header.Value := Header.Value.mk "text/html"
private def mimeTextCss        : Header.Value := Header.Value.mk "text/css"
private def mimeAppJavascript  : Header.Value := Header.Value.mk "application/javascript"
private def mimeAppJson        : Header.Value := Header.Value.mk "application/json"
private def mimeAppPdf         : Header.Value := Header.Value.mk "application/pdf"
private def mimeImagePng       : Header.Value := Header.Value.mk "image/png"
private def mimeImageJpeg      : Header.Value := Header.Value.mk "image/jpeg"
private def mimeImageGif       : Header.Value := Header.Value.mk "image/gif"
private def mimeImageSvg       : Header.Value := Header.Value.mk "image/svg+xml"
private def mimeImageWebp      : Header.Value := Header.Value.mk "image/webp"
private def mimeImageIcon      : Header.Value := Header.Value.mk "image/x-icon"
private def mimeVideoMp4       : Header.Value := Header.Value.mk "video/mp4"
private def mimeVideoWebm      : Header.Value := Header.Value.mk "video/webm"
private def mimeVideoOgg       : Header.Value := Header.Value.mk "video/ogg"
private def mimeVideoQuicktime : Header.Value := Header.Value.mk "video/quicktime"
private def mimeVideoAvi       : Header.Value := Header.Value.mk "video/x-msvideo"
private def mimeVideoMkv       : Header.Value := Header.Value.mk "video/x-matroska"
private def mimeAudioMpeg      : Header.Value := Header.Value.mk "audio/mpeg"
private def mimeAudioWav       : Header.Value := Header.Value.mk "audio/wav"
private def mimeAudioFlac      : Header.Value := Header.Value.mk "audio/flac"
private def headerBytes        : Header.Value := Header.Value.mk "bytes"

private def mimeType (path : System.FilePath) : Header.Value :=
  match path.extension with
  | "mp4"  => mimeVideoMp4
  | "webm" => mimeVideoWebm
  | "ogg"  => mimeVideoOgg
  | "mov"  => mimeVideoQuicktime
  | "avi"  => mimeVideoAvi
  | "mkv"  => mimeVideoMkv
  | "mp3"  => mimeAudioMpeg
  | "wav"  => mimeAudioWav
  | "flac" => mimeAudioFlac
  | "pdf"  => mimeAppPdf
  | "html" => mimeTextHtml
  | "css"  => mimeTextCss
  | "js"   => mimeAppJavascript
  | "json" => mimeAppJson
  | "png"  => mimeImagePng
  | "jpg"  | "jpeg" => mimeImageJpeg
  | "gif"  => mimeImageGif
  | "svg"  => mimeImageSvg
  | "webp" => mimeImageWebp
  | "ico"  => mimeImageIcon
  | "txt"  => mimeTextPlain
  | _      => mimeOctetStream

abbrev FileResponse := ContextAsync (Response Body.Any)

/--
A file on disk served with optional `Range` support for efficient seeking.

Uses the `HeaderRange` extractor to parse the `Range` request header and
returns the appropriate `206 Partial Content` with `Content-Range` for
range requests, or the full file otherwise.

`IntoResponse` is implemented with streaming and `Content-Length` framing
(not chunked transfer encoding).

```lean
def serveFile := GET "/static/{*rest}" (⟨rest⟩ : Path String) (ranges : HeaderRange) => do
  return { path := "static" / rest, ranges : RangeFile }
```
-/
structure RangeFile where
  path   : System.FilePath
  ranges : HeaderRange

private def chunkSize : Nat := 8192

private def pickRange (ranges : HeaderRange) (fileSize : Nat) : Option (Nat × Nat) :=
  match ranges.ranges with
  | none => none
  | some rs =>
    if _ : rs.size > 0 then
      let r := rs[0]!
      let (start, len) := match r.start, r.stop with
        | some s, some e =>
          if s >= fileSize then (0, 0) else
          let e := min e (fileSize - 1)
          (s, e - s + 1)
        | some s, none =>
          if s >= fileSize then (0, 0) else
          (s, fileSize - s)
        | none, some suffix =>
          let suffix := min suffix fileSize
          (fileSize - suffix, suffix)
        | none, none => (0, fileSize)
      some (start, len)
    else none

private def sendRangeStream (handle : IO.FS.Handle) (knownLen : Nat) (stream : Body.Stream) : Async Unit := do
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

private def skipBytes (handle : IO.FS.Handle) (n : Nat) : IO Unit := do
  let mut skipped := 0
  while skipped < n do
    let bytes ← handle.read (USize.ofNat (min chunkSize (n - skipped)))
    if bytes.isEmpty then break
    skipped := skipped + bytes.size

instance : IntoResponse RangeFile where
  into_response f := do
    let file ← f
    let mdata ← file.path.metadata
    let fileSize := mdata.byteSize.toNat
    let handle ← IO.FS.Handle.mk file.path .read
    match pickRange file.ranges fileSize with
    | none =>
      Response.ok
        |>.header headerContentType (mimeType file.path)
        |>.header headerAcceptRanges headerBytes
        |>.stream (sendRangeStream handle fileSize)
    | some (start, len) =>
      if start >= fileSize then
        Response.new.status .rangeNotSatisfiable
          |>.header! "content-range" s!"bytes */{mdata.byteSize}"
          |>.empty
      else
        skipBytes handle start
        let endByte := start + len - 1
        Response.new.status .partialContent
          |>.header headerContentType (mimeType file.path)
          |>.header headerAcceptRanges headerBytes
          |>.header! "content-range" s!"bytes {start}-{endByte}/{mdata.byteSize}"
          |>.stream (sendRangeStream handle len)

end LeanIO
