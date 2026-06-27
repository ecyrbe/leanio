import Leanio.Router
open Leanio
open Leanio.Router

def isOk (e : Except ε α) : Bool :=
  match e with | .ok _ => true | _ => false

def isError (e : Except ε α) (msg : ε) [BEq ε] : Bool :=
  match e with | .error m => m == msg | _ => false

def exceptOk [BEq α] (e : Except ε α) (v : α) : Bool :=
  match e with | .ok a => a == v | _ => false

-- extractParamNames
#guard extractParamNames "/user/{id}" == ["id"]
#guard extractParamNames "/posts/{year}/{month}" == ["year", "month"]
#guard extractParamNames "/files/{*path}" == ["path"]
#guard extractParamNames "/hello" == []

-- validateRoutePattern
#guard isOk (validateRoutePattern "/user/{id}")
#guard isOk (validateRoutePattern "/files/{*rest}")
#guard isOk (validateRoutePattern "/api/{*catchall}")
#guard isError (validateRoutePattern "/todos/{id") "unclosed brace in pattern"
#guard isError (validateRoutePattern "/user/{1bad}") "invalid path parameter name '1bad'"
#guard isError (validateRoutePattern "/files/{*1bad}") "invalid rest parameter name '1bad'"

-- isValidParamName
#guard isValidParamName "id"
#guard isValidParamName "_private"
#guard isValidParamName "camelCase"
#guard isValidParamName "with_digits_42"
#guard isValidParamName "A"
#guard ¬ isValidParamName ""
#guard ¬ isValidParamName "1bad"
#guard ¬ isValidParamName "my param"
#guard ¬ isValidParamName "my-param"
#guard ¬ isValidParamName "$pecial"

-- RoutePattern.ofString + length
#guard (RoutePattern.ofString "/hello").segments == [Segment.lit "hello"]
#guard (RoutePattern.ofString "/hello").length == 1
#guard (RoutePattern.ofString "/{id}").segments == [Segment.param "id"]
#guard (RoutePattern.ofString "/{id}").length == 1
#guard (RoutePattern.ofString "/user/{id}").segments == [Segment.lit "user", Segment.param "id"]
#guard (RoutePattern.ofString "/user/{id}").length == 2
#guard (RoutePattern.ofString "/{year}/{month}").segments == [Segment.param "year", Segment.param "month"]
#guard (RoutePattern.ofString "/{year}/{month}").length == 2
#guard (RoutePattern.ofString "/").segments == []
#guard (RoutePattern.ofString "/").length == 0
#guard (RoutePattern.ofString "/").hasRest == false
#guard (RoutePattern.ofString "/files/{*path}").segments == [Segment.lit "files", Segment.rest "path"]
#guard (RoutePattern.ofString "/files/{*path}").length == 2
#guard (RoutePattern.ofString "/files/{*path}").hasRest == true
#guard (RoutePattern.ofString "/static/{*any}").segments == [Segment.lit "static", Segment.rest "any"]
#guard (RoutePattern.ofString "/static/{*any}").hasRest == true

-- FromRouteParam (Nat)
#guard exceptOk (FromRouteParam.parse "42" : Except String Nat) 42
#guard isError (FromRouteParam.parse "abc" : Except String Nat) "cannot parse path param as Nat: abc"

-- FromRouteParam (String)
#guard exceptOk (FromRouteParam.parse "hello" : Except String String) "hello"

-- FromRouteParam (Bool)
#guard exceptOk (FromRouteParam.parse "true" : Except String Bool) true
#guard exceptOk (FromRouteParam.parse "false" : Except String Bool) false

-- FromRouteParam (Float) — no BEq on Float, use isOk
#guard isOk ((FromRouteParam.parse "3.14" : Except String Float))
#guard isOk ((FromRouteParam.parse "-2.5" : Except String Float))
#guard ¬ isOk ((FromRouteParam.parse "abc" : Except String Float))
