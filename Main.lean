import NixPrPatches

/-! # nix-pr-patches

A tiny CLI that adds a `nixpkgs-patch-<PR>` input to the `inputs` section
of `flake.nix` in the current directory.

usage: nix-pr-patches <PR_NUMBER>
-/

def main (args : List String) : IO Unit :=
  match args with
  | [pr] =>
    if pr = "" || !(pr.toList.all Char.isDigit) then
      IO.eprintln usage
    else
      try addPatchFile "flake.nix" pr
      catch e => IO.eprintln s!"error: could not read flake.nix: {e}"
  | _ =>
    IO.eprintln usage
