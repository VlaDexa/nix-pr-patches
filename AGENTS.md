# Nix PR patch utility

A CLI app that manages nixpkgs patches in flake.nix.

## Adding a patch: `nix-pr-patches <PR_NUMBER>`
1. We need to find inputs and find their start and end.
2. First cli argument should be a number that represents the github PR number.
3. Check whether the PR is already merged into the nixpkgs revision locked
   in flake.lock (same check as `trim` below: github api, git fallback): if
   it is, do nothing.
4. Check by url whether the PR is already in flake.nix: if
   `https://github.com/NixOS/nixpkgs/pull/${PR_NUMBER}.patch` already appears
   in flake.nix (under any input name), do nothing.
5. Otherwise we need to add an input in form of
```nix
nixpkgs-patch-${PR_NUMBER} = {
    url = "https://github.com/NixOS/nixpkgs/pull/${PR_NUMBER}.patch";
    flake = false;
};
```
6. Done

## Trimming merged patches: `nix-pr-patches trim`
1. Find all `nixpkgs-patch-*` inputs in flake.nix (start and end of each block).
2. Read flake.lock (parsed with `Lean.Data.Json` / `FromJson` / `ToJson`) to
   find the locked nixpkgs revision and branch:
   `nodes.nixpkgs.locked.rev` and `nodes.nixpkgs.original.ref`.
3. For each patch PR, ask the public github api whether the PR was merged
   (`pulls/{PR}` -> `merged_at` / `merge_commit_sha`) and whether its merge
   commit is contained in the locked revision (`compare/{mergeCommit}...{rev}`:
   `ahead`/`identical` means contained, `behind`/`diverged` means not).
   `GITHUB_TOKEN` is sent along if set (higher rate limit).
4. If the api is unavailable, fall back to git the way nixpkgs PR checkers
   do (cf. nixpk.gs): a persistent local repo with only the commit graph
   (`--filter=tree:0`, no trees or blobs), kept at
   `$XDG_CACHE_HOME/nix-pr-patches/nixpkgs` (override with
   `NIX_PR_PATCHES_NIXPKGS_REPO`), updated incrementally each run, queried
   with `git merge-base --is-ancestor`. No whole-nixpkgs download.
5. Remove the inputs whose PRs were merged into the locked revision. Keep
   anything whose status could not be determined. Afterwards run
   `nix flake lock` to clean up flake.lock.

If the agent is changing program steps, AGENTS.md should be updated to reflect that.
