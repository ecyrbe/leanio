import Lake
open System Lake DSL

/-- The LeanIO fork: the router and framework, with the LeanIn-backed native backend. The serving runtime
itself lives in `scidonia/leanin`, pinned below; this package carries the shim it links against so its
executables are built by `lake` alone. -/
package leanio where
  version := v!"0.1.0"
  fixedToolchain := true

require leanin from git "https://github.com/scidonia/leanin.git" @ "ffe3a0af70d4dee440ac1a61e23398276a5392b6"

lean_lib LeanIO where
  globs := #[.one `LeanIO, .submodules `LeanIO]

extern_lib netuv pkg := do
  let srcJob ← inputFile (pkg.dir / "c" / "netuv.c") true
  let oJob ← buildO (pkg.buildDir / "c" / "netuv.o") srcJob
    #["-fPIC", s!"-I{← getLeanIncludeDir}"] #["-O2"]
  buildStaticLib (pkg.buildDir / "lib" / nameToStaticLib "netuv") #[oJob]

def netuvLinkArgs : Array String := #["-L", ".lake/build/lib", "-lnetuv", "-luv"]

lean_exe leanioNative where
  root := `LeanIO.Examples.Native
  moreLinkArgs := netuvLinkArgs

lean_exe leanioDrain where
  root := `LeanIO.Examples.Drain
  moreLinkArgs := netuvLinkArgs

lean_exe leanioCancelContract where
  root := `LeanIO.Test.HttpCancelContract
  moreLinkArgs := netuvLinkArgs
