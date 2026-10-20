import Lean.Structure
import Lean.DeclarationRange
import Lean.Meta.Match.MatcherInfo

/-!
# Declaration graphs

Nodes are the user-facing declarations of the included modules that match `Kind`. Every other
constant of the included modules is folded: an edge `a → b` means that `a` uses `b`, directly or
through constants that are not nodes. Constants outside the included modules are never traversed.
-/

namespace DeclGraph

open Lean

/-- Which declarations become nodes. -/
inductive Kind where
  | «theorem»
  | all
  deriving BEq, Inhabited

structure Config where
  /-- Module-name prefixes that count as project code. -/
  scope : Array Name
  /-- Declarations to start from; empty means every node of the included modules. -/
  targets : Array Name := #[]
  kind : Kind := .theorem
  /-- Keep the dependents of the targets instead of their dependencies. -/
  reverse : Bool := false

structure Node where
  name : Name
  module : Name
  /-- `theorem`, `def`, `structure`, … -/
  kind : String
  /-- Line of the declaration in its module's source file. -/
  line : Nat
  target : Bool

structure Graph where
  /-- Sorted by name. -/
  nodes : Array Node
  /-- `(a, b)`: `a` uses `b`. Sorted. -/
  edges : Array (Name × Name)

/-- The name as written in the source, without the `_private.…` prefix. -/
def userName (n : Name) : Name :=
  privateToUserName n

def moduleOf? (env : Environment) (n : Name) : Option Name :=
  env.getModuleIdxFor? n |>.bind (env.header.moduleNames[·]?)

def lineOf? (env : Environment) (n : Name) : Option Nat :=
  (declRangeExt.find? (level := .exported) env n <|> declRangeExt.find? (level := .server) env n)
    |>.map (·.range.pos.line)

/-- Compiler-generated constants: auxiliary recursors, matchers, `proof_`, `eq_1`, … -/
def isGenerated (env : Environment) (n : Name) : Bool :=
  (userName n).isInternalDetail || isAuxRecursor env n || isNoConfusion env n ||
    isRecCore env n || Meta.isMatcherCore env n

def kindOf (env : Environment) : ConstantInfo → String
  | .thmInfo _ => "theorem"
  | .axiomInfo _ => "axiom"
  | .opaqueInfo _ => "opaque"
  | .quotInfo _ => "quot"
  | .recInfo _ => "recursor"
  | .ctorInfo _ => "constructor"
  | .inductInfo i => if isStructure env i.name then "structure" else "inductive"
  | .defnInfo i => if (env.getProjectionFnInfo? i.name).isSome then "field" else "def"

def inScope (env : Environment) (cfg : Config) (n : Name) : Bool :=
  (moduleOf? env n).any fun m => cfg.scope.any (·.isPrefixOf m)

/-- `n` is a node: a source declaration of the included modules matching `cfg.kind`. -/
def isNode (env : Environment) (cfg : Config) (n : Name) : Bool :=
  inScope env cfg n && !isGenerated env n && (lineOf? env n).isSome &&
    (env.find? n).any fun info => cfg.kind == .all || info.isTheorem

/-- Constants used by a declaration. An inductive type uses what its type uses, not its
constructors, which in turn use the type. -/
def usedConstants : ConstantInfo → Array Name
  | .inductInfo i => i.type.getUsedConstants
  | info => info.getUsedConstantsAsSet.toArray

/-- The nodes that `n` uses, folding every non-node constant of the included modules. -/
def deps (env : Environment) (cfg : Config) (n : Name) : NameSet := Id.run do
  let some info := env.find? n | return {}
  let mut pending := usedConstants info
  let mut seen : NameSet := ({} : NameSet).insert n
  let mut out : NameSet := {}
  while h : pending.size > 0 do
    let d := pending.back
    pending := pending.pop
    if seen.contains d then continue
    seen := seen.insert d
    if isNode env cfg d then
      out := out.insert d
    else if inScope env cfg d then
      if let some i := env.find? d then
        pending := pending ++ usedConstants i
  return out

/-- All nodes declared in the included modules. -/
def allNodes (env : Environment) (cfg : Config) : Array Name := Id.run do
  let mut out := #[]
  for mod in env.header.moduleNames, data in env.header.moduleData do
    if cfg.scope.any (·.isPrefixOf mod) then
      out := out ++ data.constNames.filter (isNode env cfg)
  return out

/-- The nodes reachable from `start` under `next`, each mapped to `next` of it. -/
def closure (start : Array Name) (next : Name → Array Name) : NameMap (Array Name) := Id.run do
  let mut out : NameMap (Array Name) := {}
  let mut pending := start
  while h : pending.size > 0 do
    let n := pending.back
    pending := pending.pop
    if out.contains n then continue
    let ns := next n
    out := out.insert n ns
    pending := pending ++ ns
  return out

def build (env : Environment) (cfg : Config) : Except String Graph := do
  for t in cfg.targets do
    unless env.contains t do throw s!"unknown declaration '{t}'"
  let edgesOf (n : Name) := (deps env cfg n).toArray
  let start := if cfg.targets.isEmpty then allNodes env cfg else cfg.targets
  let mut uses := closure start edgesOf
  if cfg.reverse then
    let all := closure (allNodes env cfg ++ cfg.targets) edgesOf
    let users := all.foldl (init := ({} : NameMap (Array Name))) fun m a bs =>
      bs.foldl (init := m) fun m b => m.insert b (((m.find? b).getD #[]).push a)
    let selected := closure start fun n => (users.find? n).getD #[]
    uses := all.filter fun n _ => selected.contains n
  let names := uses.toArray.map (·.1) |>.qsort Name.lt
  let nodes := names.map fun n =>
    { name := n
      module := (moduleOf? env n).getD .anonymous
      kind := (env.find? n).map (kindOf env) |>.getD ""
      line := (lineOf? env n).getD 0
      target := cfg.targets.contains n : Node }
  let edges := names.flatMap fun a =>
    (((uses.find? a).getD #[]).filter uses.contains |>.qsort Name.lt).map (a, ·)
  return { nodes, edges }

end DeclGraph
