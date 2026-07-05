module

namespace String

theorem find?_eq_some_imp_ne_endPos {s : String} {c : Char} {pos : s.Pos}
    (h : s.find? c = some pos) : pos ≠ s.endPos := by
  exact (String.find?_char_eq_some_iff.mp h).1

public def splitOnce (s : String) (pat : Char) : Option (String.Slice × String.Slice) :=
  match h: s.find? pat with
  | none => none
  | some pos =>
    let leftSlice  := s.sliceTo pos
    let rightSlice := s.sliceFrom (pos.next <| find?_eq_some_imp_ne_endPos h)
    some (leftSlice, rightSlice)

end String
