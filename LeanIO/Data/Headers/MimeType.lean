import Std.Http
import LeanIO.Data.String

namespace LeanIO.MimeType
open Std.Http

inductive TopLevel
  | application
  | audio
  | font
  | haptics
  | image
  | message
  | model
  | multipart
  | text
  | video

instance : ToString TopLevel where
  toString lvl := match lvl with
  | .application => "application"
  | .audio => "audio"
  | .font => "font"
  | .haptics => "haptics"
  | .image => "image"
  | .message => "message"
  | .model => "model"
  | .multipart => "multipart"
  | .text => "text"
  | .video => "video"


def octetStream    := Header.Value.mk "application/octet-stream"
def textPlain      := Header.Value.mk "text/plain"
def textHtml       := Header.Value.mk "text/html"
def textCss        := Header.Value.mk "text/css"
def textMarkdown   := Header.Value.mk "text/markdown"
def textJavascript := Header.Value.mk "text/javascript"
def imagePng       := Header.Value.mk "image/png"
def imageJpeg      := Header.Value.mk "image/jpeg"
def imageGif       := Header.Value.mk "image/gif"
def imageSvg       := Header.Value.mk "image/svg+xml"
def imageWebp      := Header.Value.mk "image/webp"
def imageIcon      := Header.Value.mk "image/x-icon"
def videoMp4       := Header.Value.mk "video/mp4"
def videoWebm      := Header.Value.mk "video/webm"
def videoOgg       := Header.Value.mk "video/ogg"
def videoQuicktime := Header.Value.mk "video/quicktime"
def videoAvi       := Header.Value.mk "video/x-msvideo"
def videoMkv       := Header.Value.mk "video/x-matroska"
def audioMpeg      := Header.Value.mk "audio/mpeg"
def audioWav       := Header.Value.mk "audio/wav"
def audioFlac      := Header.Value.mk "audio/flac"
def formUrlEncoded := Header.Value.mk "application/x-www-form-urlencoded"
def multipartForm  := Header.Value.mk "multipart/form-data"
def applicationJs  := Header.Value.mk "application/javascript"
def applicationJson:= Header.Value.mk "application/json"
def applicationPdf := Header.Value.mk "application/pdf"
def applicationZip := Header.Value.mk "application/zip"

/-- Map a MIME type string to a file extension. Returns `bin` if unknown.
    Strips parameters (everything after `;`) before matching. -/
def extForMime (mime : String) : String :=
  let mime := match mime.splitOnce ';' with
    | some (base, _) => base.trimAscii
    | none => mime.trimAscii
  match mime with
  | "text/plain"                  => "txt"
  | "text/html"                   => "html"
  | "text/css"                    => "css"
  | "text/markdown"               => "md"
  | "text/javascript"             => "js"
  | "image/png"                   => "png"
  | "image/jpeg"                  => "jpg"
  | "image/gif"                   => "gif"
  | "image/svg+xml"               => "svg"
  | "image/webp"                  => "webp"
  | "image/x-icon"                => "ico"
  | "video/mp4"                   => "mp4"
  | "video/webm"                  => "webm"
  | "video/ogg"                   => "ogv"
  | "video/quicktime"             => "mov"
  | "video/x-msvideo"             => "avi"
  | "video/x-matroska"            => "mkv"
  | "audio/mpeg"                  => "mp3"
  | "audio/wav"                   => "wav"
  | "audio/flac"                  => "flac"
  | "application/javascript"      => "js"
  | "application/json"            => "json"
  | "application/pdf"             => "pdf"
  | "application/zip"             => "zip"
  | "application/octet-stream"    => "bin"
  | "application/x-www-form-urlencoded" => "txt"
  | _ => "bin"

def mimeType (path : System.FilePath) : Header.Value :=
  match path.extension with
  | "mp4"  => MimeType.videoMp4
  | "webm" => MimeType.videoWebm
  | "ogg"  => MimeType.videoOgg
  | "mov"  => MimeType.videoQuicktime
  | "avi"  => MimeType.videoAvi
  | "mkv"  => MimeType.videoMkv
  | "mp3"  => MimeType.audioMpeg
  | "wav"  => MimeType.audioWav
  | "flac" => MimeType.audioFlac
  | "pdf"  => MimeType.applicationPdf
  | "html" => MimeType.textHtml
  | "css"  => MimeType.textCss
  | "js"   => MimeType.textJavascript
  | "json" => MimeType.applicationJson
  | "png"  => MimeType.imagePng
  | "jpg"  | "jpeg" => MimeType.imageJpeg
  | "gif"  => MimeType.imageGif
  | "svg"  => MimeType.imageSvg
  | "webp" => MimeType.imageWebp
  | "ico"  => MimeType.imageIcon
  | "txt"  => MimeType.textPlain
  | _      => MimeType.octetStream

end LeanIO.MimeType

namespace Std.Http.Headers

def hasMimeType (self : Headers) (mimeType : Header.Value) : Bool :=
  match self.get? .contentType with
  |some contentType => contentType.value.startsWith mimeType.value
  |none => false

def hasMimeTopLevel (self : Headers) (level: LeanIO.MimeType.TopLevel): Bool :=
  match self.get? .contentType with
  |some contentType => contentType.value.startsWith s!"{level}/"
  |none => false

end Std.Http.Headers

namespace LeanIO.MimeType
open Std.Http

class HasMimeTypes (α : Type) where
  mimes? : Option (List Header.Value)

def checkMimeTypes (t: Type) [HasMimeTypes t] (headers: Headers): Except String Unit := do
  match HasMimeTypes.mimes? t with
  | some mimes =>
    if mimes.any fun mime => headers.hasMimeType mime then
      return ()
    else
      throw s!"expected {" or ".intercalate <| mimes.map (·.value)}"
  | none => return ()

end LeanIO.MimeType
