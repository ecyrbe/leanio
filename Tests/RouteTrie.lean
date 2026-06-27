import Leanio.Router.RouteTrie
open Leanio.Router
open Std Http Server
open Std.Async

def h1 : HandlerSig := fun _ => default
def h2 : HandlerSig := fun _ => default
def h3 : HandlerSig := fun _ => default
def h4 : HandlerSig := fun _ => default

def captures (result : Option (List String × HandlerSig)) : List String :=
  match result with
  | some (vs, _) => vs
  | none => []

namespace Tests.RouteTrie

-- ==========================================
-- empty trie returns none
-- ==========================================
#guard (RouteTrie.lookup RouteTrie.empty .get ["anything"]).isNone
#guard (RouteTrie.lookup RouteTrie.empty .get []).isNone

-- ==========================================
-- single literal route
-- ==========================================
def trie1 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "todos"] h1

#guard (RouteTrie.lookup trie1 .get ["todos"]).isSome
#guard captures (RouteTrie.lookup trie1 .get ["todos"]) == []
#guard (RouteTrie.lookup trie1 .get ["users"]).isNone
#guard (RouteTrie.lookup trie1 .post ["todos"]).isNone

-- ==========================================
-- single param route
-- ==========================================
def trie2 := RouteTrie.empty
  |>.addRoute .get [Segment.param "id"] h1

#guard (RouteTrie.lookup trie2 .get ["42"]).isSome
#guard captures (RouteTrie.lookup trie2 .get ["42"]) == ["42"]
#guard (RouteTrie.lookup trie2 .get [""]).isSome
#guard captures (RouteTrie.lookup trie2 .get [""]) == [""]

-- ==========================================
-- literal + param in same route
-- ==========================================
def trie3 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "user", Segment.param "id"] h1

#guard (RouteTrie.lookup trie3 .get ["user", "42"]).isSome
#guard captures (RouteTrie.lookup trie3 .get ["user", "42"]) == ["42"]
#guard (RouteTrie.lookup trie3 .get ["user"]).isNone
#guard (RouteTrie.lookup trie3 .get ["admin", "42"]).isNone
#guard (RouteTrie.lookup trie3 .get ["user", "42", "extra"]).isNone

-- ==========================================
-- multiple literals, no params
-- ==========================================
def trie4 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "a", Segment.lit "b"] h1
  |>.addRoute .get [Segment.lit "a", Segment.lit "c"] h2
  |>.addRoute .get [Segment.lit "x"] h3

#guard (RouteTrie.lookup trie4 .get ["a", "b"]).isSome
#guard captures (RouteTrie.lookup trie4 .get ["a", "b"]) == []
#guard (RouteTrie.lookup trie4 .get ["a", "c"]).isSome
#guard captures (RouteTrie.lookup trie4 .get ["a", "c"]) == []
#guard (RouteTrie.lookup trie4 .get ["x"]).isSome
#guard captures (RouteTrie.lookup trie4 .get ["x"]) == []
#guard (RouteTrie.lookup trie4 .get ["a", "d"]).isNone

-- ==========================================
-- literal takes precedence over wildcard
-- ==========================================
def trie5 := RouteTrie.empty
  |>.addRoute .get [Segment.param "id"] h1
  |>.addRoute .get [Segment.lit "settings"] h2

#guard (RouteTrie.lookup trie5 .get ["settings"]).isSome
#guard captures (RouteTrie.lookup trie5 .get ["settings"]) == []
#guard (RouteTrie.lookup trie5 .get ["other"]).isSome
#guard captures (RouteTrie.lookup trie5 .get ["other"]) == ["other"]

-- ==========================================
-- multiple methods at same path
-- ==========================================
def trie6 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "items"] h1
  |>.addRoute .post [Segment.lit "items"] h2

#guard (RouteTrie.lookup trie6 .get ["items"]).isSome
#guard captures (RouteTrie.lookup trie6 .get ["items"]) == []
#guard (RouteTrie.lookup trie6 .post ["items"]).isSome
#guard captures (RouteTrie.lookup trie6 .post ["items"]) == []
#guard (RouteTrie.lookup trie6 .put ["items"]).isNone

-- ==========================================
-- full nested: /todos/{id}/comments/{cId}
-- ==========================================
def trie7 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "todos", Segment.param "id", Segment.lit "comments", Segment.param "cId"] h1

#guard (RouteTrie.lookup trie7 .get ["todos", "1", "comments", "42"]).isSome
#guard captures (RouteTrie.lookup trie7 .get ["todos", "1", "comments", "42"]) == ["1", "42"]
#guard (RouteTrie.lookup trie7 .get ["todos", "1", "comments"]).isNone
#guard (RouteTrie.lookup trie7 .get ["todos", "1", "tasks", "42"]).isNone

-- ==========================================
-- root path (empty segments)
-- ==========================================
def trie8 := RouteTrie.empty
  |>.addRoute .get [] h1

#guard (RouteTrie.lookup trie8 .get []).isSome
#guard captures (RouteTrie.lookup trie8 .get []) == []
#guard (RouteTrie.lookup trie8 .get ["anything"]).isNone

-- ==========================================
-- addRouteFromPattern (runtime construction)
-- ==========================================
def trie9 := RouteTrie.empty
  |>.addRouteFromPattern .get "/users/{uid}/posts/{pid}" h1

#guard (RouteTrie.lookup trie9 .get ["users", "10", "posts", "99"]).isSome
#guard captures (RouteTrie.lookup trie9 .get ["users", "10", "posts", "99"]) == ["10", "99"]
#guard (RouteTrie.lookup trie9 .get ["users", "10"]).isNone

-- ==========================================
-- ofRoutes from Route list
-- ==========================================
def dummyRoute (m : Method) (pat : String) (h : HandlerSig) : Route :=
  { method := m, pat := RoutePattern.ofString pat, handler := h }

def trie10 := RouteTrie.ofRoutes
  [ dummyRoute .get "/hello" h1
  , dummyRoute .put "/hello" h2
  , dummyRoute .get "/items/{item}" h3
  ]

#guard (RouteTrie.lookup trie10 .get ["hello"]).isSome
#guard captures (RouteTrie.lookup trie10 .get ["hello"]) == []
#guard (RouteTrie.lookup trie10 .put ["hello"]).isSome
#guard captures (RouteTrie.lookup trie10 .put ["hello"]) == []
#guard (RouteTrie.lookup trie10 .get ["items", "abc"]).isSome
#guard captures (RouteTrie.lookup trie10 .get ["items", "abc"]) == ["abc"]
#guard (RouteTrie.lookup trie10 .delete ["hello"]).isNone

-- ==========================================
-- catchall ({*rest}) — lowest priority
-- ==========================================
def trie11 := RouteTrie.empty
  |>.addRoute .get [Segment.rest "path"] h1

#guard (RouteTrie.lookup trie11 .get ["anything"]).isSome
#guard captures (RouteTrie.lookup trie11 .get ["anything"]) == ["anything"]
#guard (RouteTrie.lookup trie11 .get ["a", "b", "c"]).isSome
#guard captures (RouteTrie.lookup trie11 .get ["a", "b", "c"]) == ["a/b/c"]
#guard (RouteTrie.lookup trie11 .get []).isNone
#guard (RouteTrie.lookup trie11 .post ["anything"]).isNone

-- ==========================================
-- literal + catchall
-- ==========================================
def trie12 := RouteTrie.empty
  |>.addRoute .get [Segment.lit "files", Segment.rest "path"] h1

#guard (RouteTrie.lookup trie12 .get ["files", "a", "b"]).isSome
#guard captures (RouteTrie.lookup trie12 .get ["files", "a", "b"]) == ["a/b"]
#guard (RouteTrie.lookup trie12 .get ["files", "a"]).isSome
#guard captures (RouteTrie.lookup trie12 .get ["files", "a"]) == ["a"]
#guard (RouteTrie.lookup trie12 .get ["files"]).isNone
#guard (RouteTrie.lookup trie12 .get ["other"]).isNone

-- ==========================================
-- literal > wildcard > catchall priority
-- ==========================================
def trie13 := RouteTrie.empty
  |>.addRoute .get [Segment.rest "any"] h1
  |>.addRoute .get [Segment.param "id"] h2
  |>.addRoute .get [Segment.lit "settings"] h3

#guard (RouteTrie.lookup trie13 .get ["settings"]).isSome
#guard captures (RouteTrie.lookup trie13 .get ["settings"]) == []      -- literal wins
#guard (RouteTrie.lookup trie13 .get ["other"]).isSome
#guard captures (RouteTrie.lookup trie13 .get ["other"]) == ["other"]  -- wildcard wins
#guard (RouteTrie.lookup trie13 .get ["a", "b"]).isSome
#guard captures (RouteTrie.lookup trie13 .get ["a", "b"]) == ["a/b"]    -- catchall wins

-- ==========================================
-- catchall via addRouteFromPattern
-- ==========================================
def trie14 := RouteTrie.empty
  |>.addRouteFromPattern .get "/static/{*rest}" h1

#guard (RouteTrie.lookup trie14 .get ["static", "x", "y", "z"]).isSome
#guard captures (RouteTrie.lookup trie14 .get ["static", "x", "y", "z"]) == ["x/y/z"]
#guard (RouteTrie.lookup trie14 .get ["static"]).isNone

end Tests.RouteTrie
