import Std.Http

namespace LeanIO.MimeType
open Std.Http

def octetStream    : Header.Value := Header.Value.mk "application/octet-stream"
def textPlain      : Header.Value := Header.Value.mk "text/plain"
def textHtml       : Header.Value := Header.Value.mk "text/html"
def textCss        : Header.Value := Header.Value.mk "text/css"
def applicationJavascript  : Header.Value := Header.Value.mk "application/javascript"
def applicationJson        : Header.Value := Header.Value.mk "application/json"
def applicationPdf         : Header.Value := Header.Value.mk "application/pdf"
def imagePng       : Header.Value := Header.Value.mk "image/png"
def imageJpeg      : Header.Value := Header.Value.mk "image/jpeg"
def imageGif       : Header.Value := Header.Value.mk "image/gif"
def imageSvg       : Header.Value := Header.Value.mk "image/svg+xml"
def imageWebp      : Header.Value := Header.Value.mk "image/webp"
def imageIcon      : Header.Value := Header.Value.mk "image/x-icon"
def videoMp4       : Header.Value := Header.Value.mk "video/mp4"
def videoWebm      : Header.Value := Header.Value.mk "video/webm"
def videoOgg       : Header.Value := Header.Value.mk "video/ogg"
def videoQuicktime : Header.Value := Header.Value.mk "video/quicktime"
def videoAvi       : Header.Value := Header.Value.mk "video/x-msvideo"
def videoMkv       : Header.Value := Header.Value.mk "video/x-matroska"
def audioMpeg      : Header.Value := Header.Value.mk "audio/mpeg"
def audioWav       : Header.Value := Header.Value.mk "audio/wav"
def audioFlac      : Header.Value := Header.Value.mk "audio/flac"
def formUrlEncoded : Header.Value := Header.Value.mk "application/x-www-form-urlencoded"
def multipartForm  : Header.Value := Header.Value.mk "multipart/form-data"

end LeanIO.MimeType

namespace Std.Http.Headers

def hasMimeType (self : Headers) (mimeType : Header.Value) : Bool :=
  match self.get? .contentType with
  |some contentType => contentType.value.startsWith mimeType.value
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
