namespace Leanio.Utils

def formatNanos (nanos : Nat) : String :=
  if nanos ≥ 1_000_000 then s!"{nanos / 1_000_000}ms"
  else if nanos ≥ 1_000 then s!"{nanos / 1_000}µs"
  else "< 1µs"

end Leanio.Utils
