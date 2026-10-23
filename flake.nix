{
  description = "CLI to add nixpkgs PR patch inputs to flake.nix";

  inputs = {
    nixpkgs.follows = "lean4-nix/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    lean4-nix.url = "github:lenianiva/lean4-nix";
  };

  outputs =
    inputs@{
      nixpkgs,
      flake-parts,
      lean4-nix,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      perSystem =
        {
          system,
          pkgs,
          ...
        }:
        let
          lake2nix = pkgs.callPackage lean4-nix.lake { };
        in
        {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [ (lean4-nix.readToolchainFile ./lean-toolchain) ];
          };

          packages.default = lake2nix.mkPackage {
            name = "nix-pr-patches";
            src = ./.;
            propagatedBuildInputs = [
              pkgs.curl
              pkgs.git
            ];
          };

          devShells.default = pkgs.mkShell {
            packages = with pkgs.lean; [ lean-all ];
          };
        };
    };
}
