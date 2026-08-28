module

import LeanIO.Request.FromRequestParts
meta import LeanIO.Request.FromRequestParts
import LeanIO.Request.DeriveFromQuery
meta import LeanIO.Request.DeriveFromQuery
import LeanIO.Request.Form
meta import LeanIO.Request.Form
open LeanIO
open Std.Http

def parse (raw : String) : Option URI.Query :=
  match parseQuery {} raw.toUTF8.iter with
  | .success _ q => some q
  | .error _ _ => none

def lookup? (raw key : String) : Option String := do
  lookupParam (← parse raw) key

-- names encoded the way browsers do it
#guard lookup? "user%3Aname=42" "user:name" = some "42"
#guard lookup? "a%2Bb=1" "a+b" = some "1"
#guard lookup? "q%3F=x" "q?" = some "x"
#guard lookup? "a+b=1" "a b" = some "1"

-- names encoded more aggressively than any encoder would
#guard lookup? "%6Cimit=10" "limit" = some "10"

-- values
#guard lookup? "k=a%20b" "k" = some "a b"
#guard lookup? "k=a+b" "k" = some "a b"
#guard lookup? "k=" "k" = some ""
#guard lookup? "k" "k" = some ""
#guard lookup? "a=1&b=2" "b" = some "2"
#guard lookup? "a=1" "b" = none

structure Pagination where
  offset : Nat := 0
  limit : Nat := 10
deriving FromQuery

def paginate (raw : String) : Option Pagination := do
  (FromQuery.fromQuery (← parse raw)).toOption

#guard (paginate "%6Cimit=25").map (·.limit) = some 25
#guard (paginate "%6Cimit=25").map (·.offset) = some 0
