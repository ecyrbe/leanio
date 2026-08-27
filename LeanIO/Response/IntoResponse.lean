module

public import Std.Http
public import Std.Async.ContextAsync
public import Lean.Data.Json

namespace LeanIO
open Std.Http Std.Async Lean

public class IntoResponse (α : Type) where
  into_response : ContextAsync α → ContextAsync (Response Body.Any)

public class IntoResponseExt (α : Type) where
  into_response_ext : Request Body.Stream → ContextAsync α → ContextAsync (Response Body.Any)

public instance : IntoResponse (Response Body.Any) where
  into_response resp := resp

public instance : IntoResponse Unit where
  into_response u := do
    let _ ← u
    Response.ok |>.empty

public instance : IntoResponse String where
  into_response str := do
    let str ← str
    Response.ok |>.text str

public instance : IntoResponse IO.Error where
  into_response err := do
    let err ← err
    Response.internalServerError |>.text err.toString

public instance : IntoResponse Status where
  into_response status := do
    let status ← status
    Response.new.status status |>.empty

public instance [ToJson α] : IntoResponse α where
  into_response a := do
    let a ← a
    Response.ok |>.json <| Json.pretty <| toJson a

public instance [IntoResponse ε] [IntoResponse α] : IntoResponse (Except ε α) where
  into_response res := do match ← res with
    | .ok ok => IntoResponse.into_response <| pure ok
    | .error e => IntoResponse.into_response <| pure e

public instance [IntoResponse ε] [IntoResponseExt α] : IntoResponseExt (Except ε α) where
  into_response_ext req res := do match ← res with
    | .ok ok => IntoResponseExt.into_response_ext req <| pure ok
    | .error e => IntoResponse.into_response <| pure e

public instance : IntoResponse (Status × String)  where
  into_response sstr := do
    let (s, str) ← sstr
    Response.new.status s |>.text str

public instance [ToJson α] : IntoResponse (Status × α)  where
  into_response sa := do
    let (s, a) ← sa
    Response.new.status s |>.json <| Json.pretty <| toJson a

public instance [ToJson α] : IntoResponse (Status × Headers × α)  where
  into_response sha := do
    let (s, h, a) ← sha
    Response.new.status s |>.headers h |>.json <| Json.pretty <| toJson a

/--
Wraps a handler result so it can carry response extensions, the channel a middleware
reads to learn what the handler decided (a session to persist, a cookie to set).

The extensions are held as pending inserts rather than a finished map, so they are applied
on top of whatever the wrapped value's own `IntoResponse` produced instead of replacing it.

```lean4
GET "/login" => Tagged.new "welcome" |>.extension (SessionUpdate.write session)
```
-/
public structure Tagged (α : Type) where
  value : α
  tag : Extensions → Extensions := id

@[expose] public def Tagged.new (value : α) : Tagged α := { value }

@[expose] public def Tagged.extension [TypeName β] (self : Tagged α) (data : β) : Tagged α :=
  { self with tag := fun exts => (self.tag exts).insert data }

/--
Replaces the extension of type `β` with `f` applied to whatever is already there.

`extension` overwrites, which loses earlier contributions to an accumulator such as a
`Set-Cookie` list. Use this to append to one instead.
-/
@[expose] public def Tagged.modifyExtension [TypeName β] (self : Tagged α)
    (f : Option β → β) : Tagged α :=
  { self with tag := fun exts =>
      let inner := self.tag exts
      inner.insert (f (inner.get β)) }

public instance [IntoResponse α] : IntoResponse (Tagged α) where
  into_response t := do
    let t ← t
    let resp ← IntoResponse.into_response (α := α) (pure t.value)
    return { resp with extensions := t.tag resp.extensions }

public instance [IntoResponseExt α] : IntoResponseExt (Tagged α) where
  into_response_ext req t := do
    let t ← t
    let resp ← IntoResponseExt.into_response_ext (α := α) req (pure t.value)
    return { resp with extensions := t.tag resp.extensions }

end LeanIO
