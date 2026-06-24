import Leanio.Router
open Leanio
open Leanio.Router

def check (label : String) (actual : α) (expected : α) [DecidableEq α] [ToString α] : IO Unit :=
  if actual = expected then
    IO.println s!"PASS: {label}"
  else
    IO.println s!"FAIL: {label} — expected {expected}, got {actual}"

partial def checkExcept (label : String) (actual : Except String α) (expected : Except String α) [DecidableEq α] [ToString α] : IO Unit :=
  match actual, expected with
  | .ok a, .ok e => check label a e
  | .error a, .error e => check label a e
  | _, _ => IO.println s!"FAIL: {label} — expected {expected}, got {actual}"

partial def checkExceptOk (label : String) (actual : Except String α) [ToString α] : IO Unit :=
  match actual with
  | .ok _ => IO.println s!"PASS: {label}"
  | .error e => IO.println s!"FAIL: {label} — expected .ok, got .error {e}"

partial def checkExceptError (label : String) (actual : Except String α) (expectedMsg : String) [ToString α] : IO Unit :=
  match actual with
  | .error a => if a = expectedMsg then IO.println s!"PASS: {label}" else IO.println s!"FAIL: {label} — expected .error {expectedMsg}, got .error {a}"
  | .ok v => IO.println s!"FAIL: {label} — expected .error, got .ok {v}"

def main : IO Unit := do

  -- splitPath
  check "splitPath /hello" (splitPath "/hello") ["hello"]
  check "splitPath /a/b/c" (splitPath "/a/b/c") ["a", "b", "c"]
  check "splitPath /" (splitPath "/") []

  -- extractParamNames
  check "extractParamNames /user/{id}" (extractParamNames "/user/{id}") ["id"]
  check "extractParamNames /posts/{year}/{month}" (extractParamNames "/posts/{year}/{month}") ["year", "month"]
  check "extractParamNames /hello" (extractParamNames "/hello") []

  -- validateRoutePattern
  checkExcept "validateRoutePattern ok" (validateRoutePattern "/user/{id}") (.ok ())
  checkExcept "validateRoutePattern unclosed" (validateRoutePattern "/todos/{id") (.error "unclosed brace in pattern")
  checkExcept "validateRoutePattern invalid name" (validateRoutePattern "/user/{1bad}") (.error "invalid path parameter name '1bad'")

  -- isValidParamName
  check "isValidParamName id" (isValidParamName "id") true
  check "isValidParamName _private" (isValidParamName "_private") true
  check "isValidParamName camelCase" (isValidParamName "camelCase") true
  check "isValidParamName with_digits_42" (isValidParamName "with_digits_42") true
  check "isValidParamName A" (isValidParamName "A") true
  check "isValidParamName empty string" (isValidParamName "") false
  check "isValidParamName starts with digit" (isValidParamName "1bad") false
  check "isValidParamName has space" (isValidParamName "my param") false
  check "isValidParamName has hyphen" (isValidParamName "my-param") false
  check "isValidParamName starts with $pecial char" (isValidParamName "$pecial") false

  -- parsePattern
  check "parsePattern /hello" (parsePattern "/hello").parts [Sum.inl "hello"]

  -- matchPath
  check "matchPath /hello" (matchPath (parsePattern "/hello") "/hello") (some [])
  check "matchPath /user/42" (matchPath (parsePattern "/user/{id}") "/user/42") (some ["42"])
  check "matchPath not found" (matchPath (parsePattern "/user/{id}") "/hello") (none : Option (List String))

  -- stripPathPrefix
  check "stripPathPrefix /api/user /api" (stripPathPrefix "/api/user" "/api") (some "/user")
  check "stripPathPrefix /api /api" (stripPathPrefix "/api" "/api") (some "/")
  check "stripPathPrefix /apix /api" (stripPathPrefix "/apix" "/api") (none : Option String)
  check "stripPathPrefix /api/user /api/v2" (stripPathPrefix "/api/user" "/api/v2") (none : Option String)

  -- FromRouteParam
  checkExcept "parse Nat 42" ((FromRouteParam.parse : String → Except String Nat) "42") (.ok 42)
  checkExcept "parse Nat invalid" ((FromRouteParam.parse : String → Except String Nat) "abc") (.error "cannot parse path param as Nat: abc")
  checkExcept "parse String hello" ((FromRouteParam.parse : String → Except String String) "hello") (.ok "hello")
  checkExcept "parse Bool true" ((FromRouteParam.parse : String → Except String Bool) "true") (.ok true)
  checkExcept "parse Bool false" ((FromRouteParam.parse : String → Except String Bool) "false") (.ok false)
  checkExceptOk "parse Float 3.14" ((FromRouteParam.parse : String → Except String Float) "3.14")
  checkExceptOk "parse Float -2.5" ((FromRouteParam.parse : String → Except String Float) "-2.5")
  checkExceptError "parse Float invalid" ((FromRouteParam.parse : String → Except String Float) "abc") "cannot parse path param as Float: abc"

  IO.println "All tests completed."
