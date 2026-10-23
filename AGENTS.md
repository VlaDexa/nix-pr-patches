# Nix PR patch utility

A CLI app that manages nixpkgs patches in flake.nix.

## Startup: required tools
1. Before any command, check that `curl` and `git` are both on `PATH` (an
   executable of that name exists in one of its directories).
2. If either is missing, print `error: required tools not found in PATH: ...`
   naming the missing ones and exit with status 1 without touching any file.

## Adding a patch: `nix-pr-patches <PR_NUMBER> [NAME]`
1. We need to find inputs and find their start and end.
2. First cli argument should be a number that represents the github PR number.
   An optional second argument names the input `nixpkgs-patch-${NAME}` instead
   of `nixpkgs-patch-${PR_NUMBER}` (the url still uses the PR number).
3. Check whether the PR is already merged into the nixpkgs revision locked
   in flake.lock (same check as `trim` below: github api, git fallback): if
   it is, do nothing.
4. Check that flake.nix references `gepbird/nixpkgs-patcher` (matched by the
   repository reference, so all written forms count: `github:` shorthand,
   https urls, multiline blocks, pinned refs/revs, renamed inputs). If not,
   warn that the patch inputs will not be applied — but still add.
5. Check by url whether the PR is already in flake.nix: if
   `https://github.com/NixOS/nixpkgs/pull/${PR_NUMBER}.patch` already appears
   in a `nixpkgs-patch-*` input, do nothing. Inputs under other names (e.g.
   `bcachefs-nixpkgs-patch-429126`) are not managed by this tool and are
   ignored by this check.
6. Otherwise we need to add an input in form of
```nix
nixpkgs-patch-${PR_NUMBER} = {
    url = "https://github.com/NixOS/nixpkgs/pull/${PR_NUMBER}.patch";
    flake = false;
};
```
7. Done

## Trimming merged patches: `nix-pr-patches trim`
1. Find all `nixpkgs-patch-*` inputs in flake.nix (start and end of each block).
   Inputs whose name does not start with `nixpkgs-patch` (e.g.
   `bcachefs-nixpkgs-patch-429126`) are not managed and are never touched.
   The PR number of a block is read from its patch url (input names can be
   arbitrary), falling back to digits in the name.
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
