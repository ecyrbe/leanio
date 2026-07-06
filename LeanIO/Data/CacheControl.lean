module

public import Std.Http.Data.Headers.Value

namespace LeanIO.CacheControl

public inductive Directive
 | immutable
 | noCache
 | noStore
 | noTransform
 | maxAge (seconds: Nat)
 | mustRevalidate
 | proxyRevalidate
 | «private»
 | «public»
 | sMaxAge (seconds: Nat)
 | staleWhileRevalidate (seconds: Nat)
 | staleIfError (seconds: Nat)

public instance : ToString Directive where
  toString
  | .immutable => "immutable"
  | .noCache => "no-cache"
  | .noStore => "no-store"
  | .noTransform => "no-transform"
  | .maxAge seconds => s!"max-age={seconds}"
  | .mustRevalidate => "must-revalidate"
  | .proxyRevalidate => "proxy-revalidate"
  | .private => "private"
  | .public => "public"
  | .sMaxAge seconds => s!"s-max-age={seconds}"
  | .staleWhileRevalidate seconds => s!"stale-while-revalidate={seconds}"
  | .staleIfError seconds => s!"stale-if-error={seconds}"

end CacheControl

public structure CacheControl where
  directives : List CacheControl.Directive

namespace CacheControl
open Std.Http

public instance : ToString CacheControl where
  toString cc := cc.directives.map toString |> String.intercalate ", "

public instance : Coe CacheControl Header.Value where
  coe cc := Header.Value.ofString! (toString cc)

public def publicStatic (maxAge: Nat): CacheControl := ⟨[.public, .maxAge maxAge, .mustRevalidate]⟩

end LeanIO.CacheControl
