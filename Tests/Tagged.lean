module

import LeanIO
meta import LeanIO
open LeanIO
open LeanIO.Router
open Std Http Server

namespace Tests.Tagged

structure Marker where
  label : String
deriving TypeName

structure Other where
  n : Nat
deriving TypeName

/-- Accumulator-shaped, the way a `Set-Cookie` list is: several contributors each add to it. -/
structure Cookies where
  cookies : List String := []
deriving TypeName

def labelOf (exts : Extensions) : Option String := (exts.get Marker).map (·.label)

def nOf (exts : Extensions) : Option Nat := (exts.get Other).map (·.n)

def cookiesOf (exts : Extensions) : Option (List String) := (exts.get Cookies).map (·.cookies)

def kept : Extensions := Extensions.empty.insert { label := "kept" : Marker }

-- an untagged `Tagged` leaves the wrapped value's own extensions alone
#guard labelOf ((Tagged.new "x").tag kept) == some "kept"

-- tagging inserts without discarding what was already there
#guard labelOf ((Tagged.new "x" |>.extension { n := 1 : Other }).tag kept) == some "kept"
#guard nOf ((Tagged.new "x" |>.extension { n := 1 : Other }).tag kept) == some 1

-- chained extensions of different types all survive
#guard nOf ((Tagged.new "x" |>.extension { n := 1 : Other }
  |>.extension { label := "b" : Marker }).tag Extensions.empty) == some 1
#guard labelOf ((Tagged.new "x" |>.extension { n := 1 : Other }
  |>.extension { label := "b" : Marker }).tag Extensions.empty) == some "b"

-- a later extension of the same type wins
#guard nOf ((Tagged.new "x" |>.extension { n := 1 : Other }
  |>.extension { n := 2 : Other }).tag Extensions.empty) == some 2

-- `modifyExtension` sees what is already there, so a handler can add to an
-- accumulator instead of replacing it
def acc : Extensions := Extensions.empty.insert { cookies := ["first"] : Cookies }

def addCookie (c : String) : Option Cookies → Cookies
  | some prev => { cookies := prev.cookies ++ [c] }
  | none => { cookies := [c] }

#guard cookiesOf ((Tagged.new "x" |>.modifyExtension (addCookie "second")).tag acc)
  == some ["first", "second"]

#guard cookiesOf ((Tagged.new "x" |>.modifyExtension (addCookie "only")).tag Extensions.empty)
  == some ["only"]

-- the same handler using `extension` discards the earlier contribution
#guard cookiesOf ((Tagged.new "x" |>.extension { cookies := ["second"] : Cookies }).tag acc)
  == some ["second"]

-- modifying one type leaves the others alone
#guard labelOf ((Tagged.new "x" |>.modifyExtension (addCookie "c")).tag kept) == some "kept"

theorem tag_new (v : α) : (Tagged.new v).tag = id := rfl

theorem value_extension [TypeName β] (t : Tagged α) (d : β) :
    (t.extension d).value = t.value := rfl

theorem value_modifyExtension [TypeName β] (t : Tagged α) (f : Option β → β) :
    (t.modifyExtension f).value = t.value := rfl

/-!
The remaining checks are instance resolution: each route below only elaborates if
`Tagged` composes with that return shape, so compiling is the assertion.
-/

def taggedString := GET "/a" => Tagged.new "ok" |>.extension { n := 1 : Other }

def taggedStatus := GET "/b" =>
  Tagged.new (Status.created, "made") |>.extension { n := 1 : Other }

def taggedResponse := GET "/c" => do
  let r ← Response.ok |>.text "raw"
  return (Tagged.new ({ line := r.line, body := Body.Any.ofBody r.body,
                        extensions := r.extensions } : Response Body.Any)
    |>.extension { n := 1 : Other })

def taggedWithExtractor := GET "/d/{id}" (⟨id⟩ : Path Nat) =>
  pure (Tagged.new s!"user {id}" |>.extension { n := id : Other })

def taggedFile := GET "/e" =>
  Tagged.new { path := "Tests/Tagged.lean" : File } |>.extension { n := 1 : Other }

def router : Router := Router.empty
  |>.addRoute taggedString
  |>.addRoute taggedStatus
  |>.addRoute taggedResponse
  |>.addRoute taggedWithExtractor
  |>.addRoute taggedFile

end Tests.Tagged
