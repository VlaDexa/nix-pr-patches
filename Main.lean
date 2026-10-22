import NixPrPatches
import Trim

/-! # nix-pr-patches

A tiny CLI that manages nixpkgs PR patch inputs in flake.nix:

  nix-pr-patches <PR_NUMBER>   add a patch input for the PR
  nix-pr-patches trim          remove patch inputs whose PRs were merged
-/

def main (args : List String) : IO Unit :=
  match args with
  | ["trim"] => trimPatches "flake.nix" "flake.lock"
  | [pr] =>
    if pr = "" || !(pr.toList.all Char.isDigit) then
      IO.eprintln usage
    else
      try addPatchFile "flake.nix" pr
      catch e => IO.eprintln s!"error: could not read flake.nix: {e}"
  | _ =>
    IO.eprintln usage
