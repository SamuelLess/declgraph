import DeclGraph.Graph
import Lean.Data.Json

namespace DeclGraph

open Lean

/-- Source path of a module, relative to its package's source directory. -/
def sourcePath (mod : Name) : String :=
  "/".intercalate (mod.components.map (·.toString (escape := false))) ++ ".lean"

def location (n : Node) : String :=
  s!"{sourcePath n.module}:{n.line}"

def quote (s : String) : String :=
  "\"" ++ (s.replace "\\" "\\\\" |>.replace "\"" "\\\"") ++ "\""

def Graph.toDot (g : Graph) (title : String) : String := Id.run do
  let q (n : Name) := quote n.toString
  let mut lines := #[
    s!"// {g.nodes.size} nodes, {g.edges.size} edges. Arrow: declaration → declaration it uses.",
    "digraph declgraph {",
    s!"  graph [rankdir=LR, label={quote title}, labelloc=t, fontsize=20];",
    "  node [shape=box, style=\"rounded,filled\", fillcolor=\"#edf2f7\", fontname=\"sans-serif\", fontsize=10];",
    "  edge [color=\"#718096\", arrowsize=0.6];"]
  let modules := g.nodes.map (·.module) |>.qsort Name.lt |>.toList.eraseDups
  for mod in modules, i in [0:modules.length] do
    lines := lines ++ #[s!"  subgraph cluster_{i} \{", s!"    label={q mod};", "    color=\"#cbd5e0\";"]
    for n in g.nodes.filter (·.module == mod) do
      let mut attrs := #[s!"label={q (userName n.name)}", s!"tooltip={quote (location n)}"]
      if n.target then
        attrs := attrs ++ #["fillcolor=\"#fbd38d\"", "penwidth=2"]
      lines := lines.push s!"    {q n.name} [{", ".intercalate attrs.toList}];"
    lines := lines.push "  }"
  for (a, b) in g.edges do
    lines := lines.push s!"  {q a} -> {q b};"
  lines := lines.push "}"
  return "\n".intercalate lines.toList ++ "\n"

def Graph.toJson (g : Graph) (title : String) : Json :=
  let str (n : Name) := Json.str n.toString
  Json.mkObj [
    ("title", title),
    ("nodes", Json.arr <| g.nodes.map fun n => Json.mkObj [
      ("name", str n.name),
      ("label", str (userName n.name)),
      ("module", str n.module),
      ("kind", n.kind),
      ("file", sourcePath n.module),
      ("line", n.line),
      ("target", n.target)]),
    ("edges", Json.arr <| g.edges.map fun (a, b) => Json.mkObj [("from", str a), ("to", str b)])]

end DeclGraph
