import Lean

namespace Leanio

class FromRouteParam (α : Type) where
  parse : String → Except String α

instance : FromRouteParam String where
  parse s := .ok s

instance : FromRouteParam Nat where
  parse s := match s.toNat? with
    | some n => .ok n
    | none   => .error s!"cannot parse path param as Nat: {s}"

instance : FromRouteParam Int where
  parse s := match s.toInt? with
    | some n => .ok n
    | none   => .error s!"cannot parse path param as Int: {s}"

instance : FromRouteParam Bool where
  parse s := match s with
   | "true" => .ok true
   | "false" => .ok false
   | _ => .error s!"cannot parse path param as Bool: {s}"

instance : FromRouteParam Float where
  parse s :=
    match Lean.Json.parse s >>= Lean.fromJson? (α := Float) with
    | .ok f => .ok f
    | .error _ => .error s!"cannot parse path param as Float: {s}"

/--
  Extract a Subtype from a route param.

  Example:
  ```lean4
  abbrev Bounded (min max: Nat) := { x: Nat // min ≤ x ∧ x ≤ max }

  #eval (FromRouteParam.parse "42" : Except String (Bounded 0 100))-- > .ok 42
  ```
  - Notice: the predicate must be decidable and subtype should be reducible
-/
instance {α : Type} {p : α → Prop} [FromRouteParam α] [DecidablePred p] :
    FromRouteParam (Subtype p) where
  parse s := do
    let a ← FromRouteParam.parse s
    if h : p a then
      return ⟨a, h⟩
    else
      throw s!"parsed value does not satisfy subtype predicate"

end Leanio
