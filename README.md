<div align="center">

# Leanio

[![Lean](https://img.shields.io/badge/Lean-4.31.0-0f4c81)](https://lean-lang.org/)
[![Lake](https://img.shields.io/badge/build-Lake-blue)](https://github.com/leanprover/lake)
[![Version](https://img.shields.io/badge/version-0.1.0-2ea44f)](./lakefile.toml)
[![License](https://img.shields.io/badge/license-MIT-green)](./LICENSE)

A lightweight, composable HTTP router and server toolkit for Lean 4.

Built on `Std.Http.Server` with a custom routing DSL, path-parameter extraction, middleware chaining, and sub-router mounting.

</div>

## Highlights

- 🧭 **Routing DSL** — inline route definitions with `GET`, `POST`, `PUT`, `DELETE`, `PATCH`, `HEAD` macros
- 🔗 **Path parameters** — typed extraction with `{param}` syntax (`Nat`, `Int`, `String`, `Bool`, `Float`, `Subtype`)
- 🧩 **Sub-router mounting** — compose routers under path prefixes (`addRouter`)
- 🧪 **Middleware chaining** — request-level middleware with state injection via `withState`
- 📦 **JSON body parsing** — automatic `FromJson` deserialization for request bodies
- 📤 **JSON response serialization** — automatic `ToJson` serialization via `Response.json` helpers
- 🧪 **Tested** — unit tests for path splitting, pattern matching, param parsing, and prefix stripping

## Feature Snapshot

| Area | Status | Notes |
| --- | --- | --- |
| Route macros (`GET`, `POST`, etc.) | Yes | compile-time route definitions |
| Path parameter extraction | Yes | typed via `FromRouteParam` typeclass |
| JSON body parsing | Yes | via `FromRouteBody` / `FromJson` |
| JSON response serialization | Yes | via `Response.json` / `ToJson` |
| Sub-router mounting | Yes | mount routers under prefix paths |
| Middleware chaining | Yes | per-route and per-router middleware |
| State injection via extensions | Yes | inject app state into request context |
| Response helpers | Yes | JSON, created, badRequest, notFound |
| Request logging middleware | Yes | method, path, status, duration |
| Pattern validation | Yes | compile-time route pattern validation |
| Param name validation | Yes | compile-time param name validation |
| Streaming body support | Yes | raw `Body.Stream` handlers |
| Query parameters | No | not yet implemented |

## Requirements

- Lean `4.31.0`
- Lake

Toolchain is pinned in `lean-toolchain`.

## Build

```bash
lake build
```

Build and run the test target:

```bash
lake run test
```

## Quick Start

```lean
import Leanio
open Leanio.Router

GET "/hello" hello (req : Request Body.Stream) :=
  Response.ok |>.text "Hello, world!"

def main : IO Unit := Async.block do
  let addr : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, 8080⟩
  let router : Router := Router.empty
    |>.addRoute hello
  let server ← Server.serve addr router
  server.waitShutdown
```

## JSON Serialization

### Defining request and response types

Leanio uses Lean's `FromJson` and `ToJson` typeclasses for automatic JSON handling — no manual parsing or encoding required.

```lean
structure CreateUserRequest where
  name  : String
  email : String
  age   : Nat
deriving FromJson

structure UserResponse where
  id    : Nat
  name  : String
  email : String
  age   : Nat
deriving ToJson
```

`deriving FromJson` generates a JSON decoder from a `Json` value. `deriving ToJson` generates a JSON encoder. Both are provided by Lean's `Std` and work with any structure whose fields are themselves `FromJson`/`ToJson`.

### Receiving JSON in request bodies

Use `Request MyType` as the handler parameter — the body is parsed automatically:

```lean
POST "/users" createUser (req : Request CreateUserRequest) := do
  -- req.body is already parsed as CreateUserRequest
  let name  := req.body.name
  let email := req.body.email
  let age   := req.body.age
  Response.json s!"created user {name}"
```

If the JSON body is malformed or doesn't match the structure, a `400 Bad Request` is returned automatically with an error description — no manual validation needed.

### Sending JSON in responses

Use `Response.json` to serialize any `ToJson` type:

```lean
GET "/users/{id}" getUser (req : Request Body.Stream) (id : Nat) := do
  let user : UserResponse := { id, name := "Alice", email := "alice@example.com", age := 30 }
  Response.json user
```

Helper variants for common status codes:

```lean
Response.json user              -- 200 OK
Response.json.created user      -- 201 Created
Response.json.badRequest msg    -- 400 Bad Request
Response.json.notFound msg      -- 404 Not Found
```

These helpers set the status code and serialize the payload as JSON with the correct `Content-Type` header.

### Full JSON CRUD example

```lean
import Leanio.Router
open Leanio.Router

structure Pet where
  id   : Nat
  name : String
deriving ToJson

structure CreatePetRequest where
  name : String
deriving FromJson

GET "/pets" listPets (req : Request Body.Stream) := do
  -- returning an Array of pets as JSON
  Response.json #[Pet.mk 1 "Fluffy", Pet.mk 2 "Spot"]

POST "/pets" createPet (req : Request CreatePetRequest) := do
  -- req.body is auto-deserialized from JSON
  let pet := Pet.mk 3 req.body.name
  Response.json.created pet

PUT "/pets/{id}" updatePet (req : Request CreatePetRequest) (id : Nat) := do
  let updated := Pet.mk id req.body.name
  Response.json updated

DELETE "/pets/{id}" deletePet (req : Request Body.Stream) (id : Nat) := do
  Response.ok |>.text s!"pet {id} deleted"
```

### Custom JSON serialization

For types that can't use `deriving`, implement the typeclasses manually:

```lean
structure CustomDate where
  year  : Nat
  month : Nat
  day   : Nat

instance : ToJson CustomDate where
  toJson d := Json.mkObj [
    ("year",  Json.num d.year),
    ("month", Json.num d.month),
    ("day",   Json.num d.day)
  ]

instance : FromJson CustomDate where
  fromJson j := do
    let year  ← j.getObjVal? "year"  >>= Json.toNat?
    let month ← j.getObjVal? "month" >>= Json.toNat?
    let day   ← j.getObjVal? "day"   >>= Json.toNat?
    pure { year, month, day }
```

## Route definitions

Routes are defined inline with method macros:

```lean
GET "/items" listItems (req : Request Body.Stream) :=
  -- handler body, req has raw stream body

POST "/items" createItem (req : Request CreateItemRequest) :=
  -- req.body is automatically parsed from JSON as CreateItemRequest

GET "/items/{id}" getItem (req : Request Body.Stream) (id : Nat) :=
  -- id is extracted from the path and parsed as Nat

PUT "/items/{id}" updateItem (req : Request UpdateItemRequest) (id : Nat) :=
  -- both path params and JSON body parsing
```

The `Request α` type signals body parsing: `Body.Stream` for raw access, or any `FromJson` type for automatic JSON deserialization.

## Router composition

```lean
def apiV1 : Router := Router.empty
  |>.addRoute listItems
  |>.addRoute createItem

def root : Router := Router.empty
  |>.addRouter "/api/v1" apiV1
  |>.addMiddleware loggingMiddleware
```

Sub-routers strip their prefix before matching, so handler paths are relative to the mount point.

## Middleware

```lean
def myMiddleware (next : HandlerSig) : HandlerSig := fun req => do
  IO.println s!"before: {req.line.uri.path}"
  let res ← next req
  IO.println s!"after: {res.line.status}"
  return res

def router : Router := Router.empty
  |>.addRoute myRoute
  |>.addMiddleware myMiddleware
```

State injection via `withState`:

```lean
structure AppState where
  ref : IO.Ref MyData
deriving TypeName

def stateMiddleware := do
  let ref ← IO.mkRef { ... }
  return withState AppState { ref }

let router := rootRouter
  |>.addMiddleware (← stateMiddleware)
```

Extract state in handlers:

```lean
GET "/data" getData (req : Request Body.Stream) :=
  match req.extensions.get AppState with
  | some state => do
    let data ← state.ref.get
    Response.json data
  | none => Response.internalServerError |>.text "no state"
```

## Project layout

```
Leanio/           — library modules
  Router.lean     — core routing engine, macros, middleware, dispatch
  RouteParam.lean — FromRouteParam typeclass (Nat, String, Bool, Float, ...)
  RouteBody.lean  — FromRouteBody typeclass (JSON body parsing)
  Utils.lean      — formatting utilities
Leanio.lean       — library root (re-exports)
Main.lean         — entry point / example server
Tests/
  Router.lean     — unit tests for routing primitives
```

## License

MIT License. See [`LICENSE`](./LICENSE).
