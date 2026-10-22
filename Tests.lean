import Lean.Data.Json
import NixPrPatches
import Trim

open Lean

/-! # Tests for nix-pr-patches

Tests the two scenarios from AGENTS.md against dummy flake.nix contents in a
temp directory:

1. a new PR gets its input block inserted into the inputs section;
2. a PR already present (checked by url) leaves flake.nix untouched.
-/

/-- Fails the current test with a message unless `cond` holds. -/
def check (cond : Bool) (msg : String) : IO Unit :=
  if cond then pure () else throw (IO.userError msg)

/-- Index of the first line satisfying `p`, if any. -/
def findLine (ls : List String) (p : String → Bool) : Option Nat :=
  let rec go : List String → Nat → Option Nat
    | [], _ => none
    | l :: rest, i => if p l then some i else go rest (i + 1)
  go ls 0

/-- Dummy flake.nix without any patches. -/
def dummyFlake : String :=
  String.intercalate "\n"
    [ "{",
      "  description = \"Test flake\";",
      "  inputs = {",
      "    nixpkgs.url = \"github:NixOS/nixpkgs/nixos-unstable\";",
      "    flake-utils.url = \"github:numtide/flake-utils\";",
      "  };",
      "  outputs = { ... }: { };",
      "}" ] ++ "\n"

/-- Dummy flake.nix that already has a patch input for PR 12345. -/
def dummyFlakeWithPatch : String :=
  String.intercalate "\n"
    [ "{",
      "  inputs = {",
      "    nixpkgs.url = \"github:NixOS/nixpkgs\";",
      "    nixpkgs-patch-12345 = {",
      "        url = \"https://github.com/NixOS/nixpkgs/pull/12345.patch\";",
      "        flake = false;",
      "    };",
      "  };",
      "  outputs = { ... }: { };",
      "}" ] ++ "\n"

/-- Dummy flake.nix that has the patch url of PR 4242 under a different name. -/
def dummyFlakeWithOtherName : String :=
  String.intercalate "\n"
    [ "{",
      "  inputs = {",
      "    my-patch.url = \"https://github.com/NixOS/nixpkgs/pull/4242.patch\";",
      "  };",
      "  outputs = { ... }: { };",
      "}" ] ++ "\n"

/-- Runs `f` with a dummy flake.nix written to a temp directory. -/
def withDummyFlake (content : String) (f : String → IO Unit) : IO Unit :=
  IO.FS.withTempDir fun dir => do
    let path := dir.toString ++ "/flake.nix"
    IO.FS.writeFile path content
    f path

/-- Scenario 1: a new PR gets its input block inserted into the inputs section. -/
def testAddNewPatch : IO Unit := do
  withDummyFlake dummyFlake fun path => do
    addPatchFile path "12345" "12345"
    let lines ← IO.FS.lines path
    let ls := lines.toList
    check (ls.contains "    nixpkgs-patch-12345 = {") "patch block header with correct indent"
    check (ls.contains "        url = \"https://github.com/NixOS/nixpkgs/pull/12345.patch\";")
      "patch url line"
    check (ls.contains "        flake = false;") "flake = false line"
    check (ls.countP (·.contains "nixpkgs-patch-12345") == 1) "exactly one patch block"
    let headerIdx := findLine ls (· = "    nixpkgs-patch-12345 = {")
    let blockCloseIdx := findLine ls (· = "    };")
    let sectionCloseIdx := findLine ls (· = "  };")
    let outputsIdx := findLine ls (·.contains "outputs =")
    match headerIdx, blockCloseIdx, sectionCloseIdx, outputsIdx with
    | some h, some b, some s, some o =>
      check (h < b && b < s && s < o) "block sits inside the inputs section, before outputs"
    | _, _, _, _ =>
      throw (IO.userError "expected lines not found")
    -- re-adding the same PR must be a no-op
    let before ← IO.FS.readFile path
    addPatchFile path "12345" "12345"
    let after ← IO.FS.readFile path
    check (before == after) "re-adding an existing PR leaves the file unchanged"

/-- Scenario 2: a PR already present (checked by url) is left alone. -/
def testAlreadyPresent : IO Unit := do
  withDummyFlake dummyFlakeWithPatch fun path => do
    let before ← IO.FS.readFile path
    addPatchFile path "12345" "12345"
    let after ← IO.FS.readFile path
    check (before == after) "file unchanged when the PR is already present"

/-- Scenario 2 variant: the url under an unmanaged input name (not a
`nixpkgs-patch-*` block) does not block adding the managed input. -/
def testAlreadyPresentUnderOtherName : IO Unit := do
  withDummyFlake dummyFlakeWithOtherName fun path => do
    addPatchFile path "4242" "4242"
    let ls ← IO.FS.lines path
    check (ls.contains "    nixpkgs-patch-4242 = {") "managed input added despite unmanaged url"
    check (ls.contains "    my-patch.url = \"https://github.com/NixOS/nixpkgs/pull/4242.patch\";")
      "unmanaged input untouched"

/-! # Trim tests -/

/-- JSON basics: escaping, integers, objects, arrays. -/
def testJsonValues : IO Unit := do
  match Json.parse "\"a\\nb\"" with
  | .error e => throw (IO.userError s!"string parse failed: {e}")
  | .ok j => match j.getStr? with
    | .ok s => check (s == "a\nb") "string escape parsing"
    | .error e => throw (IO.userError s!"getStr? failed: {e}")
  match Json.parse "123" with
  | .error e => throw (IO.userError s!"integer parse failed: {e}")
  | .ok j => match j.getInt? with
    | .ok n => check (n == 123) "integer parsing"
    | .error e => throw (IO.userError s!"getInt? failed: {e}")
  match Json.parse "{\"a\": [1, 2, {\"b\": \"x\"}]}" with
  | .error e => throw (IO.userError s!"object parse failed: {e}")
  | .ok j => do
    match j.getObjVal? "a" with
    | .error e => throw (IO.userError e)
    | .ok v => do
      match v.getArr? with
      | .error e => throw (IO.userError e)
      | .ok arr => do
        check (arr.size == 3) "array size"
        match arr[2]!.getObjVal? "b" with
        | .error e => throw (IO.userError e)
        | .ok v2 => match v2.getStr? with
          | .ok "x" => pure ()
          | _ => throw (IO.userError "nested lookup failed")

/-- A structure with derived `FromJson`/`ToJson` instances. -/
structure RevInfo where
  rev : String
deriving FromJson, ToJson, Repr, BEq, Inhabited

/-- `deriving FromJson, ToJson` reads and writes structure fields by name. -/
def testFromToJson : IO Unit := do
  match Json.parse "{\"rev\": \"abc\"}" with
  | .error e => throw (IO.userError s!"parse failed: {e}")
  | .ok j =>
    match (fromJson? j : Except String RevInfo) with
    | .error e => throw (IO.userError s!"fromJson? failed: {e}")
    | .ok r => check (r.rev == "abc") "fromJson? on structure"
  let j := toJson { rev := "def" : RevInfo }
  match j.getObjVal? "rev" with
  | .error e => throw (IO.userError s!"toJson field missing: {e}")
  | .ok v => match v.getStr? with
    | .ok "def" => pure ()
    | _ => throw (IO.userError "toJson roundtrip failed")

/-- A flake.nix with two patch blocks. -/
def twoPatchLines : Array String :=
  #[
    "{",
    "  inputs = {",
    "    nixpkgs.url = \"github:NixOS/nixpkgs\";",
    "    nixpkgs-patch-12345 = {",
    "        url = \"https://github.com/NixOS/nixpkgs/pull/12345.patch\";",
    "        flake = false;",
    "    };",
    "    nixpkgs-patch-999 = {",
    "        url = \"https://github.com/NixOS/nixpkgs/pull/999.patch\";",
    "        flake = false;",
    "    };",
    "  };",
    "  outputs = { ... }: { };",
    "}"
  ]

/-- Patch blocks are found with the right PR numbers and line ranges. -/
def testFindPatchBlocks : IO Unit := do
  let blocks := findPatchBlocks twoPatchLines
  check (blocks.length == 2) "two blocks found"
  match blocks with
  | [b1, b2] =>
    check (b1.name == "nixpkgs-patch-12345" && b1.pr == 12345 && b1.start == 3 && b1.stop == 6)
      "first block range"
    check (b2.pr == 999 && b2.start == 7 && b2.stop == 10) "second block range"
  | _ => throw (IO.userError "wrong number of blocks")

/-- Removing the blocks leaves the rest of the file intact. -/
def testRemoveBlocks : IO Unit := do
  let remaining := removeBlocks twoPatchLines (findPatchBlocks twoPatchLines)
  check (!remaining.any (·.contains "nixpkgs-patch-")) "no patch blocks remain"
  check (remaining.size == 6) "8 lines removed (2 blocks x 4)"
  check (remaining.contains "    nixpkgs.url = \"github:NixOS/nixpkgs\";") "other input kept"
  check (remaining.contains "  };") "inputs section close preserved"
  check (remaining.any (·.contains "outputs =")) "outputs preserved"

/-- Removing nothing leaves the lines unchanged. -/
def testRemoveBlocksNoop : IO Unit := do
  check (removeBlocks twoPatchLines [] == twoPatchLines) "no blocks -> unchanged"

/-- A minimal flake.lock. -/
def dummyLock : String :=
  String.intercalate "\n"
    [ "{",
      "  \"nodes\": {",
      "    \"nixpkgs\": {",
      "      \"locked\": {",
      "        \"owner\": \"nixos\",",
      "        \"repo\": \"nixpkgs\",",
      "        \"rev\": \"1306659b587dc277866c7b69eb97e5f07864d8c4\",",
      "        \"type\": \"github\"",
      "      },",
      "      \"original\": {",
      "        \"ref\": \"nixos-unstable\",",
      "        \"repo\": \"nixpkgs\",",
      "        \"type\": \"github\"",
      "      }",
      "    }",
      "  },",
      "  \"root\": \"root\",",
      "  \"version\": 7",
      "}" ] ++ "\n"

/-- The locked nixpkgs rev is extracted from flake.lock. -/
def testNixpkgsRev : IO Unit := do
  match Json.parse dummyLock with
  | .error e => throw (IO.userError s!"lock parse failed: {e}")
  | .ok j =>
    match nixpkgsRev j with
    | some rev => check (rev == "1306659b587dc277866c7b69eb97e5f07864d8c4") "rev extracted"
    | none => throw (IO.userError "rev not found")
    match nixpkgsRef j with
    | some ref => check (ref == "nixos-unstable") "ref extracted"
    | none => throw (IO.userError "ref not found")

/-- Pulls responses are classified into merged/open. -/
def testMergedInfo : IO Unit := do
  let merged := "{ \"merged_at\": \"2024-05-01T10:00:00Z\", \"merge_commit_sha\": \"abc123\", \"state\": \"closed\" }"
  let openPr := "{ \"merged_at\": null, \"merge_commit_sha\": null, \"state\": \"open\" }"
  match Json.parse merged, Json.parse openPr with
  | .ok jm, .ok jo =>
    check (mergedInfoOf jm == some (true, some "abc123")) "merged info"
    check (mergedInfoOf jo == some (false, none)) "open info"
  | _, _ => throw (IO.userError "pulls json parse failed")

/-- Compare statuses map to trim decisions. -/
def testCompareStatusTrimmed : IO Unit := do
  check (compareStatusTrimmed "ahead" == some true) "ahead -> trim"
  check (compareStatusTrimmed "identical" == some true) "identical -> trim"
  check (compareStatusTrimmed "behind" == some false) "behind -> keep"
  check (compareStatusTrimmed "diverged" == some false) "diverged -> keep"
  check (compareStatusTrimmed "unknown" == none) "unknown -> none"

/-- The add command adds nothing when the PR is reported as merged. -/
def testAddSkipsWhenMerged : IO Unit := do
  withDummyFlake dummyFlake fun path => do
    addPatchCommandWith (fun _ => pure (some true)) path "12345" "12345"
    let after ← IO.FS.readFile path
    check (after == dummyFlake) "nothing added when the PR is already merged"

/-- The add command adds the input when the PR is not merged. -/
def testAddWhenNotMerged : IO Unit := do
  withDummyFlake dummyFlake fun path => do
    addPatchCommandWith (fun _ => pure (some false)) path "12345" "12345"
    let ls ← IO.FS.lines path
    check (ls.any (·.contains "nixpkgs-patch-12345 = {")) "patch added when the PR is not merged"

/-- Undetermined merge status falls through to adding. -/
def testAddWhenUnknown : IO Unit := do
  withDummyFlake dummyFlake fun path => do
    addPatchCommandWith (fun _ => pure none) path "12345" "12345"
    let ls ← IO.FS.lines path
    check (ls.any (·.contains "nixpkgs-patch-12345 = {")) "patch added when status is undetermined"

/-- Without a flake.lock the merged check is skipped and the patch is added. -/
def testAddWithoutLock : IO Unit := do
  IO.FS.withTempDir fun dir => do
    let path := dir.toString ++ "/flake.nix"
    IO.FS.writeFile path dummyFlake
    addPatchCommand path (dir.toString ++ "/flake.lock") "12345" "12345"
    let ls ← IO.FS.lines path
    check (ls.any (·.contains "nixpkgs-patch-12345 = {")) "patch added without a flake.lock"

/-- A custom-named patch input is added with the given name but the PR's url. -/
def testAddWithCustomName : IO Unit := do
  withDummyFlake dummyFlake fun path => do
    addPatchFile path "bump-libfoo" "12345"
    let ls ← IO.FS.lines path
    check (ls.contains "    nixpkgs-patch-bump-libfoo = {") "custom-named block header"
    check (ls.contains "        url = \"https://github.com/NixOS/nixpkgs/pull/12345.patch\";")
      "url still points at the PR"
    check (!ls.any (·.contains "nixpkgs-patch-12345")) "no default-named block"

/-- A flake.nix with a custom-named patch input. -/
def namedPatchLines : Array String :=
  #[
    "{",
    "  inputs = {",
    "    nixpkgs.url = \"github:NixOS/nixpkgs\";",
    "    nixpkgs-patch-bump-libfoo = {",
    "        url = \"https://github.com/NixOS/nixpkgs/pull/777.patch\";",
    "        flake = false;",
    "    };",
    "  };",
    "  outputs = { ... }: { };",
    "}"
  ]

/-- Trim finds the PR number of custom-named inputs from their patch url. -/
def testFindPatchBlocksNamed : IO Unit := do
  let blocks := findPatchBlocks namedPatchLines
  match blocks with
  | [b] =>
    check (b.name == "nixpkgs-patch-bump-libfoo") "custom name kept"
    check (b.pr == 777) "PR taken from the url, not the name"
    check (b.start == 3 && b.stop == 6) "block range"
  | _ => throw (IO.userError "expected exactly one block")

/-- A flake.nix with an unmanaged patch input (name does not start with
`nixpkgs-patch`) plus a managed one. -/
def mixedPatchLines : Array String :=
  #[
    "{",
    "  inputs = {",
    "    bcachefs-nixpkgs-patch-429126 = {",
    "        url = \"https://github.com/NixOS/nixpkgs/pull/429126.patch\";",
    "        flake = false;",
    "    };",
    "    nixpkgs-patch-12345 = {",
    "        url = \"https://github.com/NixOS/nixpkgs/pull/12345.patch\";",
    "        flake = false;",
    "    };",
    "  };",
    "  outputs = { ... }: { };",
    "}"
  ]

/-- A flake.nix with only an unmanaged patch input. -/
def unmanagedPatchLines : Array String :=
  #[
    "{",
    "  inputs = {",
    "    nixpkgs.url = \"github:NixOS/nixpkgs\";",
    "    bcachefs-nixpkgs-patch-429126 = {",
    "        url = \"https://github.com/NixOS/nixpkgs/pull/429126.patch\";",
    "        flake = false;",
    "    };",
    "  };",
    "  outputs = { ... }: { };",
    "}"
  ]

/-- Only inputs whose name starts with `nixpkgs-patch` are managed. -/
def testFindPatchBlocksSkipsUnmanaged : IO Unit := do
  let blocks := findPatchBlocks mixedPatchLines
  match blocks with
  | [b] => check (b.pr == 12345) "only the managed block is found"
  | _ => throw (IO.userError "expected exactly the managed block")

/-- An unmanaged input with the same patch url does not block adding the
managed input. -/
def testAddIgnoresUnmanaged : IO Unit := do
  withDummyFlake (String.intercalate "\n" unmanagedPatchLines.toList ++ "\n") fun path => do
    addPatchFile path "429126" "429126"
    let ls ← IO.FS.lines path
    check (ls.contains "    nixpkgs-patch-429126 = {") "managed input added"
    check (ls.contains "    bcachefs-nixpkgs-patch-429126 = {") "unmanaged input untouched"

/-- Removing the managed blocks leaves unmanaged inputs alone. -/
def testTrimSkipsUnmanaged : IO Unit := do
  let trimmed := removeBlocks mixedPatchLines (findPatchBlocks mixedPatchLines)
  check (trimmed.contains "    bcachefs-nixpkgs-patch-429126 = {") "unmanaged input kept"
  check (!trimmed.contains "    nixpkgs-patch-12345 = {") "managed input removed"

/-- The nixpkgs-patcher reference is found in all its written forms. -/
def testHasNixpkgsPatcher : IO Unit := do
  check (hasNixpkgsPatcher #["    nixpkgs-patcher.url = \"github:gepbird/nixpkgs-patcher\";"])
    "shorthand github scheme"
  check (hasNixpkgsPatcher #["    nixpkgs-patcher.url = \"https://github.com/gepbird/nixpkgs-patcher\";"])
    "https scheme"
  check (hasNixpkgsPatcher #["    nixpkgs-patcher.url = \"github:gepbird/nixpkgs-patcher/abc123def\";"])
    "pinned rev"
  check (hasNixpkgsPatcher #["    nixpkgs-patcher.url = \"github:gepbird/nixpkgs-patcher?ref=main\";"])
    "pinned ref"
  check (hasNixpkgsPatcher
    #[ "    nixpkgs-patcher = {",
       "      url = \"github:gepbird/nixpkgs-patcher\";",
       "      inputs.nixpkgs.follows = \"nixpkgs\";",
       "    };" ])
    "multiline block"
  check (hasNixpkgsPatcher #["    patcher.url = \"github:Gepbird/nixpkgs-patcher\";"])
    "renamed input, different case"
  check (!hasNixpkgsPatcher #[ "    nixpkgs.url = \"github:nixos/nixpkgs/nixos-unstable\";" ])
    "absent"

/-- A missing nixpkgs-patcher only warns; the patch is still added. -/
def testAddStillAddsWithoutPatcher : IO Unit := do
  withDummyFlake dummyFlake fun path => do
    addPatchCommandWith (fun _ => pure none) path "12345" "12345"
    let ls ← IO.FS.lines path
    check (ls.contains "    nixpkgs-patch-12345 = {") "patch added despite missing patcher"

def main : IO Unit := do
  testAddNewPatch
  testAlreadyPresent
  testAlreadyPresentUnderOtherName
  testJsonValues
  testFromToJson
  testFindPatchBlocks
  testRemoveBlocks
  testRemoveBlocksNoop
  testNixpkgsRev
  testMergedInfo
  testCompareStatusTrimmed
  testAddSkipsWhenMerged
  testAddWhenNotMerged
  testAddWhenUnknown
  testAddWithoutLock
  testAddWithCustomName
  testFindPatchBlocksNamed
  testFindPatchBlocksSkipsUnmanaged
  testAddIgnoresUnmanaged
  testTrimSkipsUnmanaged
  testHasNixpkgsPatcher
  testAddStillAddsWithoutPatcher
  IO.println "all tests passed"
