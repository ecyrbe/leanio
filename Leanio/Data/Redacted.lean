namespace Leanio

/-- Redacted constant string -/
abbrev REDACTED : String := "<REDACTED>"

/-- A type for representing values that should be redacted in logs and error messages. -/
structure Redacted where
  value : String
  deriving BEq, Inhabited

namespace Redacted

instance : Repr Redacted where
  reprPrec _ _ := REDACTED

instance : ToString Redacted where
  toString _ := REDACTED

instance : Coe String Redacted where
  coe str := ⟨ str ⟩

/-- Exposes the underlying value of a `Redacted` instance. -/
def expose (redacted : Redacted) : String :=
  redacted.value

end Leanio.Redacted
