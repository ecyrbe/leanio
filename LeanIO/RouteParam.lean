import Lean

namespace LeanIO

/--
Typeclass for parsing a route path parameter from a `String`.

When a route pattern contains `{param}`, the matched segment is parsed using
`FromRouteParam.parse` into the handler's argument type.

Example:
```lean4
GET "/todos/{id}" getTodo (req : Request Body.Stream) (id : Nat) := ...
-- "42" in the URL is parsed via `FromRouteParam.parse "42" : Except String Nat`
```
-/
class FromRouteParam (α : Type) where
  parse : String → Except String α

/--
  Extract String from a route param.

  Example:
  ```lean4
  #eval (FromRouteParam.parse "hello" : Except String String) -- > .ok "hello"
  ```
-/
instance : FromRouteParam String where
  parse s := .ok s

/--
  Extract natural number from a route param.

  Example:
  ```lean4
  #eval (FromRouteParam.parse "6" : Except String Nat) -- > .ok 6
  ```
-/
instance : FromRouteParam Nat where
  parse s := match s.toNat? with
    | some n => .ok n
    | none   => .error s!"cannot parse path param as Nat: {s}"

/--
  Extract an interger from a route param.

  Example:
  ```lean4
  #eval (FromRouteParam.parse "-6" : Except String Int) -- > .ok -6
  ```
-/
instance : FromRouteParam Int where
  parse s := match s.toInt? with
    | some n => .ok n
    | none   => .error s!"cannot parse path param as Int: {s}"

/--
  Extract boolean from a route param.

  Example:
  ```lean4
  #eval (FromRouteParam.parse "true" : Except String Bool) -- > .ok true
  ```
-/
instance : FromRouteParam Bool where
  parse s := match s with
   | "true" => .ok true
   | "false" => .ok false
   | _ => .error s!"cannot parse path param as Bool: {s}"

/--
  Extract float from a route param.

  Example:
  ```lean4
  #eval (FromRouteParam.parse "3.14" : Except String Float) -- > .ok 3.14
  ```
-/
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

end LeanIO
