module

import LeanIO.Utils
meta import LeanIO.Utils
open LeanIO

private def find (needle haystack : String) (start : Nat := 0) : Option Nat :=
  (Search.new needle.toUTF8).search haystack.toUTF8 start

private def overlap (needle haystack : String) : Nat :=
  (Search.new needle.toUTF8).terminalOverlap haystack.toUTF8 0

-- needles whose failure links are actually followed
#guard find "abcabd" "abcabcabd" = some 3
#guard find "aab" "aaab" = some 1
#guard find "aaa" "aaaa" = some 0

#guard find "xyz" "abcxyz" = some 3
#guard find "abc" "ababab" = none
#guard find "abcd" "abc" = none
#guard find "" "abc" = some 0
#guard find "ab" "abab" 1 = some 2

#guard overlap "abc" "xxab" = 2
#guard overlap "abc" "xxxx" = 0
