import Lean.Data.Json
import NixPrPatches

/-! # Trimming merged patches

`nix-pr-patches trim` removes `nixpkgs-patch-<PR>` inputs whose PR has been
merged into the nixpkgs revision locked in `flake.lock`.

Determining whether a PR is contained in a revision is done via the public
GitHub API when possible:

1. `pulls/{PR}` tells us whether the PR was merged and its merge commit;
2. `compare/{mergeCommit}...{rev}` tells us whether the merge commit is an
   ancestor of the locked revision (`ahead`/`identical` means it is).

If the API is unavailable, a git fallback answers the ancestry question
the way nixpkgs PR checkers do (cf. nixpk.gs): a persistent, local nixpkgs
repo containing only the commit graph (`--filter=tree:0`, so no trees or
blobs are ever downloaded — ~500MB once), updated incrementally on each
run, queried with `git merge-base --is-ancestor`. Anything that cannot be
determined is kept.

JSON handling uses Lean's `Lean.Data.Json` with `FromJson`/`ToJson`.
-/

open Lean

/-- A patch input block in flake.nix: its full input name, its PR number,
and the inclusive line range it spans. -/
structure PatchBlock where
  name : String
  pr : Nat
  start : Nat
  stop : Nat
deriving BEq, Repr

/-- The parts of a `pulls/{PR}` response we care about. -/
structure PullsResponse where
  state : String
  merged_at : Option String
  merge_commit_sha : Option String
deriving FromJson, ToJson, Repr, BEq, Inhabited

/-- The part of a `compare/{m}...{rev}` response we care about. -/
structure CompareResponse where
  status : String
deriving FromJson, ToJson, Repr, BEq, Inhabited

/-- The prefix of patch input names. -/
def patchPrefix : String := "nixpkgs-patch-"

/-- The name of the input (after the `nixpkgs-patch-` prefix) if `line`
starts a patch input block, modulo indentation. -/
def patchNameOfLine (line : String) : Option String :=
  let indent := leadingWhitespace line
  let cs := line.toList
  let pfx := String.ofList (cs.drop indent.length |>.take patchPrefix.length)
  if pfx == patchPrefix then
    let rest := cs.drop (indent.length + patchPrefix.length)
    some (String.ofList (List.takeWhile (fun c => !c.isWhitespace && c != '=' && c != '{') rest))
  else none

/-- If `line` starts a patch input block, the PR number encoded in the input
name (`nixpkgs-patch-<digits> = {`). Used as a fallback when the block has no
patch url. -/
def patchPrOfLine (line : String) : Option Nat :=
  match patchNameOfLine line with
  | none => none
  | some rest => (String.ofList (List.takeWhile (fun c => c.isDigit) rest.toList)).toNat?

/-- Extracts the PR number from a patch url line, e.g.
`url = "https://github.com/NixOS/nixpkgs/pull/12345.patch";` gives `some 12345`. -/
def patchUrlPrOfLine (line : String) : Option Nat :=
  let key := "pull/".toList
  let rec afterKey (cs : List Char) (k : List Char) : Option (List Char) :=
    match k with
    | [] => some cs
    | kc :: krest =>
      match cs with
      | c :: crest => if c == kc then afterKey crest krest else afterKey crest key
      | [] => none
  match afterKey line.toList key with
  | none => none
  | some rest =>
    let digits := List.takeWhile (fun c => c.isDigit) rest
    let suffix := rest.drop digits.length
    if digits.isEmpty then none
    else if String.ofList (suffix.take 6) == ".patch" then (String.ofList digits).toNat?
    else none

/-- The index of the line that closes the block opened at `start`. -/
partial def blockStopGo (lines : Array String) (i : Nat) (depth : Int) : Nat :=
  if depth <= 0 || i + 1 >= lines.size then i
  else
    let depth' := depth + braceCount lines[i + 1]!
    if depth' <= 0 then i + 1
    else blockStopGo lines (i + 1) depth'

def blockStop (lines : Array String) (start : Nat) : Nat :=
  if start >= lines.size then start
  else blockStopGo lines start (braceCount lines[start]!)

/-- Looks for a patch url in lines `i`..`stop`. -/
partial def blockPrFromUrl (lines : Array String) (stop : Nat) (i : Nat) : Option Nat :=
  if i > stop then none
  else
    match patchUrlPrOfLine lines[i]! with
    | some n => some n
    | none => blockPrFromUrl lines stop (i + 1)

/-- The PR number of the block spanning lines `start`..`stop`, taken from the
block's patch url (custom-named inputs encode the PR there); falls back to
digits in the input name. -/
def blockPr (lines : Array String) (start stop : Nat) : Option Nat :=
  match blockPrFromUrl lines stop (start + 1) with
  | some n => some n
  | none => patchPrOfLine lines[start]!

/-- All `nixpkgs-patch-*` input blocks in `lines`, in order of appearance.
Blocks whose PR number cannot be determined are skipped. -/
partial def findPatchBlocksGo (lines : Array String) (i : Nat) (acc : List PatchBlock) :
    List PatchBlock :=
  if i >= lines.size then acc.reverse
  else
    match patchNameOfLine lines[i]! with
    | some namePart =>
      let stop := blockStop lines i
      match blockPr lines i stop with
      | some pr =>
        findPatchBlocksGo lines (stop + 1)
          ({ name := s!"nixpkgs-patch-{namePart}", pr := pr, start := i, stop := stop } :: acc)
      | none => findPatchBlocksGo lines (stop + 1) acc
    | none => findPatchBlocksGo lines (i + 1) acc

def findPatchBlocks (lines : Array String) : List PatchBlock :=
  findPatchBlocksGo lines 0 []

/-- Removes the given (sorted, disjoint) line ranges from `lines`. -/
def removeBlocks (lines : Array String) (blocks : List PatchBlock) : Array String :=
  let rec go (ls : List String) (i : Nat) (bs : List PatchBlock) : List String :=
    match bs with
    | [] => ls
    | b :: rest =>
      let kept := ls.take (b.start - i)
      let restLs := ls.drop (b.stop + 1 - i)
      kept ++ go restLs (b.stop + 1) rest
  (go lines.toList 0 blocks).toArray

/-- The locked nixpkgs revision from a parsed flake.lock, if present. -/
def nixpkgsRev (lock : Json) : Option String :=
  (do
    let nodes ← lock.getObjVal? "nodes"
    let np ← nodes.getObjVal? "nixpkgs"
    let locked ← np.getObjVal? "locked"
    let rev ← locked.getObjVal? "rev"
    rev.getStr?
  ).toOption

/-- The nixpkgs ref from a parsed flake.lock (e.g. `nixos-unstable`),
if present. -/
def nixpkgsRef (lock : Json) : Option String :=
  (do
    let nodes ← lock.getObjVal? "nodes"
    let np ← nodes.getObjVal? "nixpkgs"
    let orig ← np.getObjVal? "original"
    let ref ← orig.getObjVal? "ref"
    ref.getStr?
  ).toOption

/-- `(merged, merge_commit_sha)` from a `pulls/{PR}` response. -/
def mergedInfoOf (j : Json) : Option (Bool × Option String) :=
  match (fromJson? j : Except String PullsResponse) with
  | .error _ => none
  | .ok r => some (r.merged_at.isSome, r.merge_commit_sha)

/-- Whether a compare status means the patch commit is contained in the
revision: `some true` trim, `some false` keep, `none` unknown. -/
def compareStatusTrimmed : String → Option Bool
  | "ahead" => some true
  | "identical" => some true
  | "behind" => some false
  | "diverged" => some false
  | _ => none

/-- GETs `url` via curl, sending `GITHUB_TOKEN` along if it is set. -/
def curlGet (url : String) : IO (Except String String) := do
  let token ← IO.getEnv "GITHUB_TOKEN"
  let args := match token with
    | some t => #["-fsSL", "-H", s!"Authorization: Bearer {t}", url]
    | none => #["-fsSL", url]
  let out ← IO.Process.output { cmd := "curl", args := args }
  if out.exitCode == 0 then pure (Except.ok out.stdout)
  else pure (Except.error s!"curl failed (exit {out.exitCode}): {out.stderr}")

/-- `curl` for the `pulls/{PR}` endpoint, parsed into
`(merged, merge_commit_sha)`; `none` on any failure. -/
def fetchPulls (pr : Nat) : IO (Option (Bool × Option String)) := do
  let url := s!"https://api.github.com/repos/NixOS/nixpkgs/pulls/{pr}"
  match ← curlGet url with
  | .error e => do
    IO.eprintln s!"  warning: could not fetch PR {pr}: {e}"
    pure none
  | .ok body =>
    match Json.parse body with
    | .error e => do
      IO.eprintln s!"  warning: could not parse response for PR {pr}: {e}"
      pure none
    | .ok j => pure (mergedInfoOf j)

/-- `curl` for the `compare/{m}...{rev}` endpoint, parsed into a trim
decision; `none` if the endpoint failed or the status is unknown. -/
def fetchCompare (m rev : String) : IO (Option Bool) := do
  let url := s!"https://api.github.com/repos/NixOS/nixpkgs/compare/{m}...{rev}"
  match ← curlGet url with
  | .error _ => pure none
  | .ok body =>
    match Json.parse body with
    | .error _ => pure none
    | .ok j =>
      match (fromJson? j : Except String CompareResponse) with
      | .error _ => pure none
      | .ok r => pure (compareStatusTrimmed r.status)

/-- The nixpkgs remote url. -/
def nixpkgsUrl : String := "https://github.com/NixOS/nixpkgs"

/-- Where the git fallback keeps its persistent nixpkgs commit-graph repo.

Overridable with the `NIX_PR_PATCHES_NIXPKGS_REPO` environment variable;
defaults to `$XDG_CACHE_HOME/nix-pr-patches/nixpkgs`. -/
def nixpkgsRepoDir : IO String := do
  match ← IO.getEnv "NIX_PR_PATCHES_NIXPKGS_REPO" with
  | some d => pure d
  | none => do
    let base ← match ← IO.getEnv "XDG_CACHE_HOME" with
      | some d => pure d
      | none => do
        match ← IO.getEnv "HOME" with
        | some h => pure (h ++ "/.cache")
        | none => pure ".cache"
    pure (base ++ "/nix-pr-patches/nixpkgs")

/-- Ensures the persistent nixpkgs repo exists and is up to date with the
commit graph of ref `ref`. Commits only: `--filter=tree:0` downloads no
trees or blobs, and later runs only fetch the new commits. -/
def ensureNixpkgsRepo (ref : String) : IO (Except String String) := do
  let dir ← nixpkgsRepoDir
  let _ ← IO.FS.createDirAll dir
  let init ← IO.Process.output { cmd := "git", args := #["-C", dir, "init", "-q"] }
  if init.exitCode != 0 then
    return Except.error s!"git init failed: {init.stderr}"
  let setUrl ← IO.Process.output
    { cmd := "git", args := #["-C", dir, "remote", "set-url", "origin", nixpkgsUrl] }
  if setUrl.exitCode != 0 then
    let add ← IO.Process.output
      { cmd := "git", args := #["-C", dir, "remote", "add", "origin", nixpkgsUrl] }
    if add.exitCode != 0 then
      return Except.error s!"git remote add failed: {add.stderr}"
  let fetch ← IO.Process.output
    { cmd := "git",
      args := #["-C", dir, "fetch", "-q", "--no-tags", "--filter=tree:0", "origin",
        s!"+refs/heads/{ref}:refs/remotes/origin/{ref}"] }
  if fetch.exitCode != 0 then
    return Except.error s!"git fetch failed: {fetch.stderr}"
  pure (Except.ok dir)

/-- git fallback, nixpk.gs-style: is merge commit `m` an ancestor of `rev`?
The ancestry query is answered from the persistent commit-graph repo. -/
def gitAncestor (ref rev m : String) : IO (Option Bool) := do
  match ← ensureNixpkgsRepo ref with
  | .error e => do
    IO.eprintln s!"  warning: git fallback failed: {e}"
    pure none
  | .ok dir => do
    -- `m` is usually already in the fetched history (a no-op fetch); if not,
    -- git negotiates and fetches only the missing ancestry.
    let _ ← IO.Process.output
      { cmd := "git",
        args := #["-C", dir, "fetch", "-q", "--no-tags", "--filter=tree:0", "origin", m] }
    let out ← IO.Process.output
      { cmd := "git", args := #["-C", dir, "merge-base", "--is-ancestor", m, rev] }
    if out.exitCode == 0 then pure (some true)
    else if out.exitCode == 1 then pure (some false)
    else do
      IO.eprintln s!"  warning: git merge-base failed: {out.stderr}"
      pure none

/-- Whether PR `pr`'s patch is contained in nixpkgs revision `rev`:
`some true`/`some false`, or `none` when it could not be determined.
`ref` is the nixpkgs branch from flake.lock (for the git fallback). -/
def patchInRev (ref : String) (pr : Nat) (rev : String) : IO (Option Bool) := do
  IO.println s!"checking PR {pr} against {rev}..."
  match ← fetchPulls pr with
  | none => pure none
  | some (false, _) => pure (some false)
  | some (true, none) => do
    IO.eprintln s!"  warning: PR {pr} is merged but has no merge_commit_sha"
    pure none
  | some (true, some m) =>
    match ← fetchCompare m rev with
    | some b => pure (some b)
    | none => do
      IO.eprintln "  falling back to git..."
      gitAncestor ref rev m

/-- Whether PR `pr` is merged into the nixpkgs revision locked in `lockPath`:
`some true`/`some false`, or `none` when it could not be determined
(e.g. the lock is missing, unparseable, or the check failed). -/
def prMergedIntoLock (lockPath : String) (pr : Nat) : IO (Option Bool) := do
  let content ←
    try some <$> IO.FS.readFile lockPath
    catch _ => pure none
  match content with
  | none => do
    IO.eprintln s!"warning: no {lockPath}, skipping merged check"
    pure none
  | some c =>
    match Json.parse c with
    | .error e => do
      IO.eprintln s!"warning: could not parse {lockPath}: {e}, skipping merged check"
      pure none
    | .ok j =>
      match nixpkgsRev j with
      | none => do
        IO.eprintln s!"warning: no locked nixpkgs revision in {lockPath}, skipping merged check"
        pure none
      | some rev => patchInRev (nixpkgsRef j |>.getD "master") pr rev

/-- The add command's decision logic, parameterized over the merged check
for testability: if the PR is already merged into the locked nixpkgs,
nothing is added; otherwise the patch input (named `nixpkgs-patch-<name>`)
is added as usual. -/
def addPatchCommandWith (mergedCheck : Nat → IO (Option Bool)) (flakePath : String)
    (name : String) (pr : String) : IO Unit := do
  let n := (pr.toNat?).get!
  match ← mergedCheck n with
  | some true =>
    IO.println (s!"PR {n} is already merged into the locked nixpkgs, nothing to add" ++
      " (run `nix-pr-patches trim` to clean up existing patch inputs)")
  | _ => addPatchFile flakePath name pr

/-- Adds a patch input for `pr` to `flakePath`, unless the PR is already
merged into the nixpkgs revision locked in `lockPath`. The input is named
`nixpkgs-patch-<name>`. -/
def addPatchCommand (flakePath lockPath : String) (name : String) (pr : String) : IO Unit :=
  addPatchCommandWith (fun n => prMergedIntoLock lockPath n) flakePath name pr

/-- Removes patch inputs whose PRs are merged into the locked nixpkgs
revision. -/
def trimPatches (flakePath lockPath : String) : IO Unit := do
  let lines ← IO.FS.lines flakePath
  let blocks := findPatchBlocks lines
  if blocks.isEmpty then
    IO.println "no nixpkgs-patch inputs found, nothing to trim"
    return
  let lock ← IO.FS.readFile lockPath
  match Json.parse lock with
  | .error e => IO.eprintln s!"error: could not parse {lockPath}: {e}"
  | .ok lockJson =>
    match nixpkgsRev lockJson with
    | none => IO.eprintln s!"error: no locked nixpkgs revision found in {lockPath}"
    | some rev =>
      let ref := (nixpkgsRef lockJson).getD "master"
      IO.println s!"nixpkgs: ref {ref}, revision {rev} (from {lockPath})"
      let rec go (bs : List PatchBlock) (toRemove : List PatchBlock) : IO Unit :=
        match bs with
        | [] =>
          if toRemove.isEmpty then
            IO.println "nothing to trim"
          else do
            let newLines := removeBlocks lines toRemove.reverse
            IO.FS.writeFile flakePath (String.intercalate "\n" newLines.toList ++ "\n")
            IO.println s!"trimmed {toRemove.length} patch input(s) from {flakePath}"
            IO.println "run `nix flake lock` to clean up flake.lock"
        | b :: rest => do
          let r ← patchInRev ref b.pr rev
          match r with
          | none => do
            IO.println s!"  keeping {b.name} (could not determine status)"
            go rest toRemove
          | some false => do
            IO.println s!"  keeping {b.name} (not merged into {rev})"
            go rest toRemove
          | some true => do
            IO.println s!"  removing {b.name} (merged into {rev})"
            go rest (b :: toRemove)
      go blocks []
