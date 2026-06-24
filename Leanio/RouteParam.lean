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

end Leanio
