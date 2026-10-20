import DeclGraph
-- Makes `lake test` build the fixture before running.
import DeclGraphTest

/-! `lake test`: checks the graphs of the `DeclGraphTest` fixture. -/

open Lean DeclGraph

/-- Edges as `a -> b`, with source names. -/
def render (g : Graph) : List String :=
  g.edges.toList.map fun (a, b) => s!"{userName a} -> {userName b}"

def cases : List (String × Config × List String) := [
  ("theorems", { scope := #[`DeclGraphTest] }, [
    "Fixture.double_mul -> Fixture.double_eq",
    "Fixture.quad -> Fixture.double_mul",
    "Fixture.small_double -> Fixture.double_eq",
    -- through the definition `twice`
    "Fixture.twice_spec -> Fixture.double_eq"]),
  ("target", { scope := #[`DeclGraphTest], targets := #[`Fixture.quad] }, [
    "Fixture.double_mul -> Fixture.double_eq",
    "Fixture.quad -> Fixture.double_mul"]),
  ("reverse", { scope := #[`DeclGraphTest], targets := #[`Fixture.double_mul], reverse := true }, [
    "Fixture.quad -> Fixture.double_mul"]),
  ("one module", { scope := #[`DeclGraphTest.Use], targets := #[`Fixture.quad] }, []),
  ("all", { scope := #[`DeclGraphTest], kind := .all, targets := #[`Fixture.x_mk, `Fixture.pred_double] }, [
    "Fixture.Point.mk -> Fixture.Point",
    "Fixture.Point.x -> Fixture.Point",
    "Fixture.pred_double -> Fixture.double",
    "Fixture.pred_double -> Fixture.pred",
    "Fixture.x_mk -> Fixture.Point.mk",
    "Fixture.x_mk -> Fixture.Point.x"])]

def main : IO UInt32 := do
  initSearchPath (← findSysroot)
  let env ← importModules #[{ module := `DeclGraphTest }] {}
  let mut failed := false
  for (label, cfg, expected) in cases do
    let .ok g := build env cfg | throw <| IO.userError s!"{label}: build failed"
    let actual := render g
    if actual.toArray.qsort (· < ·) != expected.toArray.qsort (· < ·) then
      failed := true
      IO.eprintln s!"FAIL {label}\n  expected: {expected}\n  actual:   {actual}"
  -- Node attributes: private names lose their prefix, generated constants never appear, fields
  -- are fields even when `Prop`-valued, and lines are those of the declaration's name.
  let .ok g := build env { scope := #[`DeclGraphTest], kind := .all } | throw <| IO.userError "all"
  let attrs := g.nodes.map fun n => (userName n.name, n.kind, n.line)
  for expected in [(`Fixture.double_eq, "theorem", 9), (`Fixture.double_mul, "theorem", 13),
      (`Fixture.Small.le, "field", 25), (`Fixture.Point.x, "field", 4)] do
    unless attrs.contains expected do
      failed := true
      IO.eprintln s!"FAIL node {expected.1}: {attrs.find? (·.1 == expected.1)}"
  if attrs.any (·.1.toString.contains "match_") then
    failed := true
    IO.eprintln s!"FAIL generated nodes: {attrs.map (·.1)}"
  -- Golden output: the committed example.
  let .ok g := build env { scope := #[`DeclGraphTest] } | throw <| IO.userError "golden"
  let dot := g.toDot "Declaration dependencies"
  if dot != (← IO.FS.readFile "examples/fixture.dot") then
    failed := true
    IO.eprintln "FAIL examples/fixture.dot differs; regenerate it as described in the README"
  if failed then return 1
  IO.println "all tests passed"
  return 0
