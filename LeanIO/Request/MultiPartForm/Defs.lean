import Lean
import Std.Async.ContextAsync
import Std.Data.ByteSlice
import LeanIO.Utils
import LeanIO.Data.ChunkBuffer

namespace LeanIO
open Std.Http Std.Async Std.Slice

inductive Phase : Type where
  | ready
  | inFile
  | done

instance : Inhabited Phase where
  default := .ready

structure MultipartInner : Type where
  cb             : ChunkBuffer := ChunkBuffer.empty
  pos            : Nat := 0
  boundStart     : ByteArray
  boundSepSearch : Search ChunkBuffer
  stream         : Body.Stream
  phase          : Phase := .ready
  nameCounter    : Nat := 0

structure FormFile where
  name        : String
  filename    : String
  contentType : String
  headers     : Headers
  inner       : IO.Ref MultipartInner

inductive MultipartEntry : Type where
  | field (name : String) (value : String)
  | file  (file : FormFile)

structure MultiPartForm where
  inner : IO.Ref MultipartInner

abbrev crlf : ByteArray := "\r\n".toUTF8
abbrev endMarker : ByteArray := "--\r\n".toUTF8

end LeanIO
