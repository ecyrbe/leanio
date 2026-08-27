module

import LeanIO.Router.RouteTrie
meta import LeanIO.Router.RouteTrie
open LeanIO.Router
open Std Http Server
open Std.Async

def h1 : HandlerFn := fun _ => default
def h2 : HandlerFn := fun _ => default
def h3 : HandlerFn := fun _ => default
def h4 : HandlerFn := fun _ => default

def getCap (result : RouteTrie.Lookup) (key : String) : String :=
  match result with
  | .found vs _ => vs.find? (·.1 == key) |>.map (·.2) |>.getD ""
  | _ => ""

def nCaps (result : RouteTrie.Lookup) : Nat :=
  match result with
  | .found vs _ => vs.length
  | _ => 0

namespace Tests.RouteTrie

-- ==========================================
-- empty trie returns none
-- ==========================================
#guard (RouteTrie.lookup RouteTrie.empty .get ["anything"]).toOption.isNone
#guard (RouteTrie.lookup RouteTrie.empty .get []).toOption.isNone

-- ==========================================
-- single literal route
-- ==========================================
def trie1 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "todos"] h1

#guard (RouteTrie.lookup trie1 .get ["todos"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie1 .get ["todos"]) == 0
#guard (RouteTrie.lookup trie1 .get ["users"]).toOption.isNone
#guard (RouteTrie.lookup trie1 .post ["todos"]).toOption.isNone

-- ==========================================
-- single param route
-- ==========================================
def trie2 := RouteTrie.empty
  |>.addRoute .get [Segment.param "id"] h1

#guard (RouteTrie.lookup trie2 .get ["42"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie2 .get ["42"]) "id" == "42"
-- an empty segment is not a parameter value: `/todos/` decodes to ["todos", ""],
-- and binding "" reported a parse failure for a parameter never sent
#guard (RouteTrie.lookup trie2 .get [""]).toOption.isNone

-- ==========================================
-- literal + param in same route
-- ==========================================
def trie3 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "user", Segment.param "id"] h1

#guard (RouteTrie.lookup trie3 .get ["user", "42"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie3 .get ["user", "42"]) "id" == "42"
#guard (RouteTrie.lookup trie3 .get ["user"]).toOption.isNone
#guard (RouteTrie.lookup trie3 .get ["admin", "42"]).toOption.isNone
#guard (RouteTrie.lookup trie3 .get ["user", "42", "extra"]).toOption.isNone

-- ==========================================
-- multiple literals, no params
-- ==========================================
def trie4 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "a", Segment.lit "b"] h1
  |>.addRoute .get [Segment.lit "a", Segment.lit "c"] h2
  |>.addRoute .get [Segment.lit "x"] h3

#guard (RouteTrie.lookup trie4 .get ["a", "b"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie4 .get ["a", "b"]) == 0
#guard (RouteTrie.lookup trie4 .get ["a", "c"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie4 .get ["a", "c"]) == 0
#guard (RouteTrie.lookup trie4 .get ["x"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie4 .get ["x"]) == 0
#guard (RouteTrie.lookup trie4 .get ["a", "d"]).toOption.isNone

-- ==========================================
-- literal takes precedence over wildcard
-- ==========================================
def trie5 := RouteTrie.empty
  |>.addRoute .get [Segment.param "id"] h1
  |>.addRoute .get [Segment.lit "settings"] h2

#guard (RouteTrie.lookup trie5 .get ["settings"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie5 .get ["settings"]) == 0
#guard (RouteTrie.lookup trie5 .get ["other"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie5 .get ["other"]) "id" == "other"

-- ==========================================
-- multiple methods at same path
-- ==========================================
def trie6 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "items"] h1
  |>.addRoute .post [Segment.lit "items"] h2

#guard (RouteTrie.lookup trie6 .get ["items"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie6 .get ["items"]) == 0
#guard (RouteTrie.lookup trie6 .post ["items"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie6 .post ["items"]) == 0
#guard (RouteTrie.lookup trie6 .put ["items"]).toOption.isNone

-- ==========================================
-- full nested: /todos/{id}/comments/{cId}
-- ==========================================
def trie7 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "todos", Segment.param "id", Segment.lit "comments", Segment.param "cId"] h1

#guard (RouteTrie.lookup trie7 .get ["todos", "1", "comments", "42"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie7 .get ["todos", "1", "comments", "42"]) "id" == "1"
#guard getCap (RouteTrie.lookup trie7 .get ["todos", "1", "comments", "42"]) "cId" == "42"
#guard (RouteTrie.lookup trie7 .get ["todos", "1", "comments"]).toOption.isNone
#guard (RouteTrie.lookup trie7 .get ["todos", "1", "tasks", "42"]).toOption.isNone

-- ==========================================
-- root path (empty segments)
-- ==========================================
def trie8 := RouteTrie.empty
  |>.addRoute .get [] h1

#guard (RouteTrie.lookup trie8 .get []).toOption.isSome
#guard nCaps (RouteTrie.lookup trie8 .get []) == 0
#guard (RouteTrie.lookup trie8 .get ["anything"]).toOption.isNone

-- ==========================================
-- addRouteFromPattern (runtime construction)
-- ==========================================
def trie9 := RouteTrie.empty
  |>.addRouteFromPattern .get "/users/{uid}/posts/{pid}" h1

#guard (RouteTrie.lookup trie9 .get ["users", "10", "posts", "99"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie9 .get ["users", "10", "posts", "99"]) "uid" == "10"
#guard getCap (RouteTrie.lookup trie9 .get ["users", "10", "posts", "99"]) "pid" == "99"
#guard (RouteTrie.lookup trie9 .get ["users", "10"]).toOption.isNone

-- ==========================================
-- ofRoutes from Route list
-- ==========================================
def dummyRoute (m : Method) (pat : String) (h : HandlerFn) : Route :=
  { method := m, pat := RoutePattern.ofString pat, handler := h }

def trie10 := RouteTrie.ofRoutes
  [ dummyRoute .get "/hello" h1
  , dummyRoute .put "/hello" h2
  , dummyRoute .get "/items/{item}" h3
  ]

#guard (RouteTrie.lookup trie10 .get ["hello"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie10 .get ["hello"]) == 0
#guard (RouteTrie.lookup trie10 .put ["hello"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie10 .put ["hello"]) == 0
#guard (RouteTrie.lookup trie10 .get ["items", "abc"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie10 .get ["items", "abc"]) "item" == "abc"
#guard (RouteTrie.lookup trie10 .delete ["hello"]).toOption.isNone

-- ==========================================
-- catchall ({*rest}) — lowest priority
-- ==========================================
def trie11 := RouteTrie.empty
  |>.addRoute .get [Segment.rest "path"] h1

#guard (RouteTrie.lookup trie11 .get ["anything"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie11 .get ["anything"]) "path" == "anything"
#guard (RouteTrie.lookup trie11 .get ["a", "b", "c"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie11 .get ["a", "b", "c"]) "path" == "a/b/c"
#guard (RouteTrie.lookup trie11 .get []).toOption.isNone
#guard (RouteTrie.lookup trie11 .post ["anything"]).toOption.isNone

-- ==========================================
-- literal + catchall
-- ==========================================
def trie12 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "files", Segment.rest "path"] h1

#guard (RouteTrie.lookup trie12 .get ["files", "a", "b"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie12 .get ["files", "a", "b"]) "path" == "a/b"
#guard (RouteTrie.lookup trie12 .get ["files", "a"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie12 .get ["files", "a"]) "path" == "a"
#guard (RouteTrie.lookup trie12 .get ["files"]).toOption.isNone
#guard (RouteTrie.lookup trie12 .get ["other"]).toOption.isNone

-- ==========================================
-- literal > wildcard > catchall priority
-- ==========================================
def trie13 := RouteTrie.empty
  |>.addRoute .get [Segment.rest "any"] h1
  |>.addRoute .get [Segment.param "id"] h2
  |>.addRoute .get [Segment.lit "settings"] h3

#guard (RouteTrie.lookup trie13 .get ["settings"]).toOption.isSome
#guard nCaps (RouteTrie.lookup trie13 .get ["settings"]) == 0
#guard (RouteTrie.lookup trie13 .get ["other"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie13 .get ["other"]) "id" == "other"
#guard (RouteTrie.lookup trie13 .get ["a", "b"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie13 .get ["a", "b"]) "any" == "a/b"

-- ==========================================
-- catchall via addRouteFromPattern
-- ==========================================
def trie14 := RouteTrie.empty
  |>.addRouteFromPattern .get "/static/{*rest}" h1

#guard (RouteTrie.lookup trie14 .get ["static", "x", "y", "z"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie14 .get ["static", "x", "y", "z"]) "rest" == "x/y/z"
#guard (RouteTrie.lookup trie14 .get ["static"]).toOption.isNone

end Tests.RouteTrie

-- ==========================================
-- trailing slash
-- ==========================================
def trie15 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "todos"] h1
  |>.addRoute .get [Segment.lit "todos", Segment.param "id"] h2

#guard (RouteTrie.lookup trie15 .get ["todos"]).toOption.isSome
#guard (RouteTrie.lookup trie15 .get ["todos", "1"]).toOption.isSome
#guard getCap (RouteTrie.lookup trie15 .get ["todos", "1"]) "id" == "1"
#guard (RouteTrie.lookup trie15 .get ["todos", ""]).toOption.isNone
#guard (RouteTrie.lookup trie15 .get ["", ""]).toOption.isNone

-- ==========================================
-- method mismatch vs no match
-- ==========================================
def allowedOf (r : RouteTrie.Lookup) : List Method :=
  match r with | .methodMismatch ms => ms | _ => []

def isNoMatch (r : RouteTrie.Lookup) : Bool :=
  match r with | .noMatch => true | _ => false

def trie16 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "todos"] h1
  |>.addRoute .put [Segment.lit "todos"] h2

#guard (RouteTrie.lookup trie16 .get ["todos"]).toOption.isSome

-- a registered path under an unregistered method reports what it does accept.
-- `HashMap.keys` has no specified order, so assert membership rather than a list
#guard (allowedOf (RouteTrie.lookup trie16 .post ["todos"])).contains .get
#guard (allowedOf (RouteTrie.lookup trie16 .post ["todos"])).contains .put
#guard (allowedOf (RouteTrie.lookup trie16 .post ["todos"])).length == 2

-- an unregistered path is a different answer entirely
#guard isNoMatch (RouteTrie.lookup trie16 .get ["nope"])
#guard isNoMatch (RouteTrie.lookup trie16 .post ["nope"])

-- HEAD is implied by GET, matching what axum advertises
#guard RouteTrie.allowValue [.get] == "GET,HEAD"
#guard RouteTrie.allowValue [.get, .head] == "GET,HEAD"
#guard RouteTrie.allowValue [.post] == "POST"
#guard RouteTrie.allowValue [.get, .put] == "GET,PUT,HEAD"

-- HEAD is served by GET only; a path with no GET still reports a mismatch
def trie17 := RouteTrie.empty
  |>.addRoute .post [Segment.lit "submit"] h1

#guard (allowedOf (RouteTrie.lookup trie17 .head ["submit"])).contains .post
#guard ¬ (allowedOf (RouteTrie.lookup trie17 .head ["submit"])).contains .get
