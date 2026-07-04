import Std.Http

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
      throw s!"expected {(mimes.map (·.value)).intersperse " or "}"
  | none => return ()

end LeanIO.MimeType
