import NixPrPatches
import Trim

/-! # nix-pr-patches

A tiny CLI that manages nixpkgs PR patch inputs in flake.nix:

  nix-pr-patches <PR_NUMBER> [NAME]   add a patch input for the PR
  nix-pr-patches trim                 remove patch inputs whose PRs were merged
-/

def main (args : List String) : IO Unit := do
  unless (← isOnPath "curl") do
    IO.eprintln "error: required tool not found in PATH: curl"
    IO.Process.exit 1
  match args with
  | ["trim"] => trimPatches "flake.nix" "flake.lock"
  | [pr] =>
    if pr = "" || !(pr.toList.all Char.isDigit) then
      IO.eprintln usage
    else
      try addPatchCommand "flake.nix" "flake.lock" pr pr
      catch e => IO.eprintln s!"error: {e}"
  | [pr, name] =>
    if pr = "" || !(pr.toList.all Char.isDigit) || name = "" then
      IO.eprintln usage
    else
      try addPatchCommand "flake.nix" "flake.lock" name pr
      catch e => IO.eprintln s!"error: {e}"
  | _ =>
    IO.eprintln usage
