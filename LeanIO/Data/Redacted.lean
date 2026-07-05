module

namespace LeanIO

/-- Redacted constant string -/
public abbrev REDACTED : String := "<REDACTED>"

/-- A type for representing values that should be redacted in logs and error messages. -/
public structure Redacted where
  value : String
  deriving BEq, Inhabited

namespace Redacted

public instance : Repr Redacted where
  reprPrec _ _ := REDACTED

public instance : ToString Redacted where
  toString _ := REDACTED

public instance : Coe String Redacted where
  coe str := ⟨ str ⟩

/-- Exposes the underlying value of a `Redacted` instance. -/
public def expose (redacted : Redacted) : String :=
  redacted.value

end LeanIO.Redacted
