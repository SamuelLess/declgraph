import Lake.Load.Workspace
import Lake.Config.LeanLib

/-! Defaults read from the Lake configuration of the current directory. -/

namespace DeclGraph

open Lean Lake

structure Defaults where
  /-- Root modules of the package's default library targets (all libraries if there are none). -/
  roots : Array Name
  /-- Root modules of all the package's libraries. -/
  scope : Array Name

def loadDefaults : IO Defaults := do
  let (elan?, some lean, some lake) ← findInstall?
    | throw <| IO.userError "could not find the Lean installation"
  let lakeEnv ← match ← (Env.compute lake lean elan?).toBaseIO with
    | .ok env => pure env
    | .error e => throw <| IO.userError e
  let some ws ← (loadWorkspaceRoot { lakeEnv, wsDir := ← IO.currentDir }).toBaseIO
    | throw <| IO.userError "could not load the Lake configuration of the current directory"
  let libs := ws.root.leanLibs
  let defaults := libs.filter (ws.root.defaultTargets.contains ·.name)
  return {
    roots := (if defaults.isEmpty then libs else defaults).flatMap (·.roots)
    scope := libs.flatMap (·.roots) }

end DeclGraph
