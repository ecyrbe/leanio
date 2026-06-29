
def HList : List (Type u) → (Type u) := List.foldr Prod PUnit

namespace HList

@[match_pattern]
abbrev nil : HList [] := ⟨⟩

@[match_pattern]
abbrev cons (x : τ) (xs : HList τs) : HList (τ :: τs) := (x, xs)

def rec {motive : (τs : List (Type u)) → HList τs → Sort u}
    (nil : motive [] HList.nil)
    (cons : {τ : Type u} → {τs : List (Type u)} → (x : τ) → (xs : HList τs) →
              motive τs xs → motive (τ :: τs) (HList.cons x xs))
    {τs : List (Type u)} (xs : HList τs) : motive τs xs :=
  match τs, xs with
  | [], PUnit.unit => nil
  | _ :: _, (x, xs) => cons x xs (HList.rec nil cons xs)

class AllRepr (τs : List Type) where
  reprElems : HList τs → List Std.Format

instance : AllRepr [] where
  reprElems _ := []

instance [Repr τ] [AllRepr τs] : AllRepr (τ :: τs) where
  reprElems elms:= match elms with
    | ⟨x, xs⟩ => repr x :: AllRepr.reprElems xs

instance [AllRepr τs] : Repr (HList τs) where
  reprPrec xs _ :=
    match AllRepr.reprElems xs with
    | [] => "[]ₕ"
    | l  =>
        let inner :=  l.intersperse ", " |> Std.Format.join
        "[" ++ inner ++ "]ₕ"

instance [AllRepr τs] : ToString (HList τs) where
  toString xs := reprStr xs

class AllBEq (τs : List Type) where
  beqElems : HList τs → HList τs → Bool

instance : AllBEq [] where
  beqElems _ _ := true

instance [BEq τ] [AllBEq τs] : AllBEq (τ :: τs) where
  beqElems xs ys :=
    match xs, ys with
    | ⟨x, xs⟩, ⟨y, ys⟩ =>
        (x == y) && AllBEq.beqElems xs ys

instance [AllBEq τs] : BEq (HList τs) where
  beq xs ys := AllBEq.beqElems xs ys

infixr:67 " ::ₕ " => HList.cons

syntax (name := hlistCons) "[" term,* "]ₕ" : term
macro_rules (kind := hlistCons)
  | `([]ₕ)          => `(HList.nil)
  | `([$x]ₕ)        => `(HList.cons $x []ₕ)
  | `([$x, $xs,*]ₕ) => `(HList.cons $x [$xs,*]ₕ)

def length : {τs : List (Type u)} → HList τs → Nat
 | τs, _ => τs.length

def head : HList (τ :: τs) → τ
 | (x, _) => x

def tail : HList (τ :: τs) → HList τs
 | (_, xs) => xs

def get : {τs : List (Type u)} → (i : Fin τs.length) → HList τs → τs[i]
| _ :: _, ⟨0, _⟩, (x, xs) => x
| _ :: _, ⟨i+1, hi⟩, (x, xs) => HList.get ⟨i, by grind only [= List.length_cons]⟩ xs

abbrev get' {τs : List (Type u)}  (i : Nat) (h: i< τs.length := by decide): HList τs → τs[i] :=
  HList.get ⟨i, h⟩

def hAppend : {αs βs : List (Type u)} → HList αs → HList βs → HList (αs ++ βs)
  | []  , _, nil, ys => ys
  | _::_, _, (x, xs), ys => (x, hAppend xs ys)

instance : HAppend (HList αs) (HList βs) (HList (αs ++ βs)) where
  hAppend := hAppend

end HList

namespace Function

/-
A Varying parameters Function type
Useful to cast any function when applying it to a HList
-/
@[reducible]
def Fn : List (Type u) → Type u → Type u
| [],      R => R
| A :: AS, R => A → Fn AS R

infix:30 " →ₕ " => Function.Fn

/--
Apply HList to normal function

Example:
```lean4
def f [ToString α] [ToString β] [ToString γ] : α → β → γ → String :=
  fun n s b =>
    s!"{n} {s} {b}"

#eval Function.apply f [1, "hi", true]ₕ
-- output: "1 hi true"
```
-/
def apply :
  {as : List (Type u)} →
  {r : Type u} →
  as →ₕ r → HList as → r
| [],      _, f, ⟨⟩ => f
| _ :: as, r, f, (x, xs) => apply (as := as) (r := r) (f x) xs

end Function
