/-! # nix-pr-patches: core logic

The pure and IO logic for adding `nixpkgs-patch-<PR>` inputs to a flake.nix.
Kept in a library module so both the executable (`Main`) and the tests
(`Tests`) can import it.
-/

def usage : String :=
  "usage: nix-pr-patches <PR_NUMBER> [NAME]   add a patch input for the PR\n" ++
  "       nix-pr-patches trim                 remove patch inputs merged into nixpkgs"

/-- The leading whitespace of a line, used to keep indentation consistent. -/
def leadingWhitespace (s : String) : String :=
  String.ofList (s.toList.takeWhile fun c => c = ' ' || c = '\t')

/-- The lines to insert for one patch input. `indent` is the indentation of
entries inside the `inputs` section. The input is named `nixpkgs-patch-<name>`
and patches PR `pr`. -/
def inputBlock (name : String) (pr : String) (indent : String) : List String :=
  [ s!"{indent}nixpkgs-patch-{name} = \{",
    s!"{indent}    url = \"https://github.com/NixOS/nixpkgs/pull/{pr}.patch\";",
    s!"{indent}    flake = false;",
    indent ++ "};" ]

/-- The net number of opening braces on a line (`#`{` - #`}`). -/
def braceCount (s : String) : Int :=
  s.foldl (fun n c =>
    if c = '{' then n + 1
    else if c = '}' then n - 1
    else n) 0

/-- The prefix of managed patch input names. Inputs whose name does not start
with this (e.g. `bcachefs-nixpkgs-patch-429126`) are not managed by this tool. -/
def patchPrefix : String := "nixpkgs-patch-"

/-- The patch url for a PR. -/
def patchUrl (pr : String) : String :=
  s!"https://github.com/NixOS/nixpkgs/pull/{pr}.patch"

/-- Whether the line (modulo indentation) starts a managed patch input block
(`nixpkgs-patch-...`). -/
def isPatchBlockStart (line : String) : Bool :=
  let indent := leadingWhitespace line
  let cs := line.toList
  String.ofList (cs.drop indent.length |>.take patchPrefix.length) == patchPrefix

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

/-- Whether any line in `start`..`stop` contains `needle`. -/
partial def blockContainsGo (lines : Array String) (needle : String) (stop : Nat) (i : Nat) :
    Bool :=
  if i > stop then false
  else if lines[i]!.contains needle then true
  else blockContainsGo lines needle stop (i + 1)

def blockContains (lines : Array String) (start stop : Nat) (needle : String) : Bool :=
  blockContainsGo lines needle stop start

/-- Whether PR `pr`'s patch url already appears in a managed `nixpkgs-patch-*`
input block. Inputs with the same url under other names are not managed and
are ignored. -/
partial def patchUrlInManagedBlocksGo (url : String) (lines : Array String) (i : Nat) : Bool :=
  if i >= lines.size then false
  else if isPatchBlockStart lines[i]! then
    let stop := blockStop lines i
    if blockContains lines i stop url then true
    else patchUrlInManagedBlocksGo url lines (stop + 1)
  else patchUrlInManagedBlocksGo url lines (i + 1)

def patchUrlInManagedBlocks (pr : String) (lines : Array String) : Bool :=
  patchUrlInManagedBlocksGo (patchUrl pr) lines 0

/-- Finds the `inputs = { ... };` section in `flake.nix`.

Returns `(start, stop)`: the line index of `inputs = {` and of its
closing `};`. Brace nesting is tracked so that closing braces of nested
input blocks are not mistaken for the end of the section. -/
partial def findInputsSectionGo (lines : Array String) (i : Nat) (start : Option Nat) (depth : Int) :
    Option (Nat × Nat) :=
  if i >= lines.size then none
  else
    let line := lines[i]!
    match start with
    | none =>
      if line.contains "inputs = {" then
        findInputsSectionGo lines (i + 1) (some i) (braceCount line)
      else
        findInputsSectionGo lines (i + 1) none 0
    | some s =>
      let depth' := depth + braceCount line
      if depth' == 0 then some (s, i)
      else findInputsSectionGo lines (i + 1) (some s) depth'

def findInputsSection (lines : Array String) : Option (Nat × Nat) :=
  findInputsSectionGo lines 0 none 0

/-- The possible outcomes of applying a patch to the lines of a flake.nix. -/
inductive PatchResult where
  | present (url : String)
  | added (newLines : Array String) (start stop : Nat)
  | noInputs

/-- Pure core of the patch-adding logic: checks by url whether PR `pr` is
already present in a managed `nixpkgs-patch-*` input, and otherwise inserts
the input block named `nixpkgs-patch-<name>`. -/
def applyPatch (name : String) (pr : String) (lines : Array String) : PatchResult :=
  let url := patchUrl pr
  -- the PR is considered present only if its patch url appears in a managed
  -- `nixpkgs-patch-*` input block; unmanaged inputs (e.g. named
  -- `bcachefs-nixpkgs-patch-*`) with the same url do not count
  if patchUrlInManagedBlocks pr lines then
    PatchResult.present url
  else
    match findInputsSection lines with
    | none => PatchResult.noInputs
    | some (start, stop) =>
      -- indent new entries by two more spaces than the section's closing brace
      let indent := leadingWhitespace lines[stop]! ++ "  "
      let ls := lines.toList
      let newLs := ls.take stop ++ inputBlock name pr indent ++ ls.drop stop
      PatchResult.added newLs.toArray start stop

/-- Adds a patch input named `nixpkgs-patch-<name>` for PR `pr` to the flake
at `path`. -/
def addPatchFile (path : String) (name : String) (pr : String) : IO Unit := do
  let lines ← IO.FS.lines path
  match applyPatch name pr lines with
  | .present url => IO.println s!"PR {pr} is already in flake.nix ({url}), nothing to do"
  | .noInputs => IO.eprintln s!"error: no `inputs = \{` section found in {path}"
  | .added newLines start stop =>
    IO.FS.writeFile path (String.intercalate "\n" newLines.toList ++ "\n")
    IO.println s!"added nixpkgs-patch-{name} to {path} (inputs section: lines {start + 1}-{stop + 1})"
