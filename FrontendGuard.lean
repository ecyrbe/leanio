module

import LeanIO
meta import Lean.Elab.Command

/-!
# Frontend guard

`Lean.*` is the compiler frontend. If anything LeanIO links imports part of it without
`meta`, the frontend lands in the runtime closure of every client, taking a linked binary
from ~6 MB to ~126 MB.

Nothing about that is visible while writing the offending import: the code compiles and the
tests pass, and the cost only appears when someone links an executable. This module turns it
into a build error instead..

Two checks:

1. **The import closure**, read from compiled `.olean` metadata. It cannot see *private*
   imports, though, so:
2. **The library sources**, scanned for a non-`meta` import of `Lean.*`.
-/

open Lean Elab Command

meta partial def walkLean (dir : System.FilePath) : IO (Array System.FilePath) := do
  let mut out := #[]
  for e in (← dir.readDir) do
    if (← e.path.isDir) then
      out := out ++ (← walkLean e.path)
    else if e.path.extension == some "lean" then
      out := out.push e.path
  return out

/--
Non-`meta` imports of `Lean.*` in one source file, as `(line number, line)`.
Skips comments so that prose mentioning an import cannot trip the check.
-/
meta def badImports (path : System.FilePath) : IO (Array (Nat × String)) := do
  let mut bad := #[]
  let mut inComment := false
  let mut lineNo := 0
  for line in (← IO.FS.readFile path).splitOn "\n" do
    lineNo := lineNo + 1
    let t := line.trimAscii.toString
    if inComment then
      if (t.splitOn "-/").length > 1 then inComment := false
    else if t.startsWith "--" then
      continue
    else if t.startsWith "/-" then
      if (t.splitOn "-/").length == 1 then inComment := true
    else
      let ws := (t.splitOn " ").filter (· != "")
      let ws := if ws.head? == some "public" then ws.tail else ws
      -- `meta import` is the legitimate form: available while elaborating, never linked.
      if ws.head? == some "meta" then continue
      if ws.head? != some "import" then continue
      let rest := ws.tail
      let rest := if rest.head? == some "all" then rest.tail else rest
      if let some m := rest.head? then
        if m == "Lean" || m.startsWith "Lean." then
          bad := bad.push (lineNo, t)
  return bad

run_cmd do
  let mut problems : Array MessageData := #[]

  -- 1. The compiled import closure, including dependencies.
  let h := (← getEnv).header
  for i in [0:h.modules.size] do
    let name := h.modules[i]!.module
    -- The frontend may import itself however it likes.
    if (`Lean).isPrefixOf name then continue
    if _ : i < h.moduleData.size then
      for imp in h.moduleData[i].imports do
        if (`Lean).isPrefixOf imp.module && !imp.isMeta then
          problems := problems.push m!"  {name} imports {imp.module} without `meta`"

  -- 2. LeanIO's own sources, for the privately-imported modules check 1 cannot see.
  -- Located relative to this file so that the check still works when LeanIO is built as
  -- somebody else's dependency, where the working directory is their package root.
  let root := (System.FilePath.mk (← getFileName)).parent.getD "."
  let srcDir := root / "LeanIO"
  if ← srcDir.isDir then
    for f in (← walkLean srcDir) do
      for (lineNo, line) in ← badImports f do
        problems := problems.push m!"  {f}:{lineNo}: {line}"

  unless problems.isEmpty do
    throwError "\
      The compiler frontend is in LeanIO's runtime closure, which will take a linked \
      binary from ~6 MB to ~126 MB:\n\n\
      {MessageData.joinSep problems.toList "\n"}\n\n\
      Drop the import if it is unused, or make it `meta import` if a macro or deriving \
      handler needs it — `meta` imports are available while elaborating but are never \
      linked. See §7.4 of the README."
