module

meta import LeanIO.Response.File.Utils
open LeanIO

-- Every rejected case below served an arbitrary file before this guard existed.

-- ordinary fragments
#guard isContainableFragment "index.html"
#guard isContainableFragment "css/style.css"
#guard isContainableFragment "."
#guard isContainableFragment "file..name.txt"
#guard isContainableFragment "..hidden"

-- parent traversal, including one that stays inside but is still refused
#guard ¬ isContainableFragment ".."
#guard ¬ isContainableFragment "../../../lakefile.toml"
#guard ¬ isContainableFragment "a/../../../../lakefile.toml"
#guard ¬ isContainableFragment "a/b/../c"

-- absolute fragments discard the root: `root / "/etc/passwd"` is `/etc/passwd`,
-- reachable from one percent-encoded slash
#guard ¬ isContainableFragment "/etc/passwd"
#guard ¬ isContainableFragment "/"

-- the joined result, not just the predicate
#guard resolveUnder "static" "css/style.css" = some "static/css/style.css"
#guard resolveUnder "static" "../../etc/passwd" = none
#guard resolveUnder "static" "/etc/passwd" = none

-- without a root, an absolute path is indistinguishable from a deliberate one
#guard resolveServePath none "/var/data/x" = some "/var/data/x"
#guard resolveServePath none "static/../x" = none
#guard resolveServePath (some "static") "a.txt" = some "static/a.txt"
