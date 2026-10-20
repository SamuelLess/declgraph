import DeclGraph

open Lean DeclGraph

def usage : String := "\
Usage: lake exe declgraph [OPTIONS] OUT.{dot,json}

Draws the declaration-level dependency graph of the current package.
An edge A -> B means that A uses B, possibly through declarations that are not nodes.

Options:
  --root M1,M2       modules to import (default: the package's default library targets)
  --target D1,D2     declarations to start from (default: every node)
  --include P1,P2    module prefixes that count as project code (default: the package's libraries)
  --kind theorem|all which declarations become nodes (default: theorem)
  --reverse          keep the dependents of the targets instead of their dependencies
  --title T          graph title
  --help             show this message"

structure Args where
  root : Option (Array Name) := none
  target : Array Name := #[]
  scope : Option (Array Name) := none
  kind : Kind := .theorem
  reverse : Bool := false
  title : Option String := none
  out : Option System.FilePath := none

def names (s : String) : Array Name :=
  (s.splitOn ",").filter (!·.isEmpty) |>.map String.toName |>.toArray

def parse : Args → List String → Except String Args
  | a, [] => pure a
  | a, "--root" :: v :: r => parse { a with root := names v } r
  | a, "--target" :: v :: r => parse { a with target := names v } r
  | a, "--include" :: v :: r => parse { a with scope := names v } r
  | a, "--kind" :: "theorem" :: r => parse { a with kind := .theorem } r
  | a, "--kind" :: "all" :: r => parse { a with kind := .all } r
  | a, "--reverse" :: r => parse { a with reverse := true } r
  | a, "--title" :: v :: r => parse { a with title := v } r
  | a, f :: r =>
    if f.startsWith "-" || a.out.isSome then throw s!"unexpected argument '{f}'"
    else parse { a with out := some f } r

def defaultTitle (a : Args) : String :=
  let targets := ", ".intercalate (a.target.map toString).toList
  if a.target.isEmpty then "Declaration dependencies"
  else if a.reverse then s!"Dependents of {targets}"
  else s!"Dependencies of {targets}"

def main (argv : List String) : IO UInt32 := do
  if argv.contains "--help" || argv.contains "-h" then
    IO.println usage
    return 0
  let a ← match parse {} argv with
    | .ok a => pure a
    | .error e => IO.eprintln s!"{e}\n\n{usage}"; return 1
  let some out := a.out | IO.eprintln usage; return 1
  let some ext := out.extension.filter (· ∈ ["dot", "json"])
    | IO.eprintln "the output file must end in .dot or .json"; return 1
  let defaults ← if a.root.isNone || a.scope.isNone then some <$> loadDefaults else pure none
  let roots := a.root.getD (defaults.map (·.roots) |>.getD #[])
  let scope := a.scope.getD (defaults.map (·.scope) |>.getD #[])
  initSearchPath (← findSysroot)
  let env ← importModules (roots.map ({ module := · })) {} (trustLevel := 1024)
  let cfg := { scope, targets := a.target, kind := a.kind, reverse := a.reverse }
  let g ← match build env cfg with
    | .ok g => pure g
    | .error e => IO.eprintln e; return 1
  let title := a.title.getD (defaultTitle a)
  IO.FS.writeFile out <| if ext == "dot" then g.toDot title else (g.toJson title).pretty ++ "\n"
  IO.println s!"{out}: {g.nodes.size} nodes, {g.edges.size} edges"
  return 0
