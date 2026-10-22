/-! # nix-pr-patches: core logic

The pure and IO logic for adding `nixpkgs-patch-<PR>` inputs to a flake.nix.
Kept in a library module so both the executable (`Main`) and the tests
(`Tests`) can import it.
-/

def usage : String :=
  "usage: nix-pr-patches <PR_NUMBER>   add a patch input for the PR\n" ++
  "       nix-pr-patches trim          remove patch inputs merged into nixpkgs"

/-- The leading whitespace of a line, used to keep indentation consistent. -/
def leadingWhitespace (s : String) : String :=
  String.ofList (s.toList.takeWhile fun c => c = ' ' || c = '\t')

/-- The lines to insert for one patch input. `indent` is the indentation of
entries inside the `inputs` section. -/
def inputBlock (pr : String) (indent : String) : List String :=
  [ s!"{indent}nixpkgs-patch-{pr} = \{",
    s!"{indent}    url = \"https://github.com/NixOS/nixpkgs/pull/{pr}.patch\";",
    s!"{indent}    flake = false;",
    indent ++ "};" ]

/-- The net number of opening braces on a line (`#`{` - #`}`). -/
def braceCount (s : String) : Int :=
  s.foldl (fun n c =>
    if c = '{' then n + 1
    else if c = '}' then n - 1
    else n) 0

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

/-- The patch url for a PR. -/
def patchUrl (pr : String) : String :=
  s!"https://github.com/NixOS/nixpkgs/pull/{pr}.patch"

/-- The possible outcomes of applying a patch to the lines of a flake.nix. -/
inductive PatchResult where
  | present (url : String)
  | added (newLines : Array String) (start stop : Nat)
  | noInputs

/-- Pure core of the patch-adding logic: checks by url whether the PR is
already present, and otherwise inserts the input block. -/
def applyPatch (pr : String) (lines : Array String) : PatchResult :=
  let url := patchUrl pr
  -- the PR is considered present if its patch url already appears anywhere,
  -- regardless of the input name it is stored under
  if lines.any (·.contains url) then
    PatchResult.present url
  else
    match findInputsSection lines with
    | none => PatchResult.noInputs
    | some (start, stop) =>
      -- indent new entries by two more spaces than the section's closing brace
      let indent := leadingWhitespace lines[stop]! ++ "  "
      let ls := lines.toList
      let newLs := ls.take stop ++ inputBlock pr indent ++ ls.drop stop
      PatchResult.added newLs.toArray start stop

/-- Adds a patch input for `pr` to the flake at `path`. -/
def addPatchFile (path : String) (pr : String) : IO Unit := do
  let lines ← IO.FS.lines path
  match applyPatch pr lines with
  | .present url => IO.println s!"PR {pr} is already in flake.nix ({url}), nothing to do"
  | .noInputs => IO.eprintln s!"error: no `inputs = \{` section found in {path}"
  | .added newLines start stop =>
    IO.FS.writeFile path (String.intercalate "\n" newLines.toList ++ "\n")
    IO.println s!"added nixpkgs-patch-{pr} to {path} (inputs section: lines {start + 1}-{stop + 1})"
