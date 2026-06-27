import LeanIO.Utils
open LeanIO.Utils

-- standard padded
#guard base64DecodeString "" = some ""
#guard base64DecodeString "SGVsbG8=" = some "Hello"
#guard base64DecodeString "aGVsbG8=" = some "hello"
#guard base64DecodeString "AA==" = some "\x00"
#guard base64DecodeString "AAA=" = some "\x00\x00"
#guard base64DecodeString "AAAA" = some "\x00\x00\x00"
#guard base64DecodeString "TWFu" = some "Man"
#guard base64DecodeString "TWF=" = some "Ma"
#guard base64DecodeString "TH=" = some "L"

-- non-padded
#guard base64DecodeString "SGVsbG8" = some "Hello"
#guard base64DecodeString "AA" = some "\x00"
#guard base64DecodeString "AAA" = some "\x00\x00"
#guard base64DecodeString "TWFu" = some "Man"
#guard base64DecodeString "TWF" = some "Ma"
#guard base64DecodeString "TH" = some "L"

-- url-safe (same as standard when no + or / used)
#guard base64DecodeString "SGVsbG8" = some "Hello"
#guard base64DecodeString "aGVsbG8=" = some "hello"

-- invalid
#guard base64DecodeString "!!!" = none
#guard base64DecodeString "abc" = none
