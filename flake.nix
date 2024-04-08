{
  description = "MAPPL Compiler";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; config.allowUnfree = true; config.allowUnsupportedSystem = true; config.allowBroken = true; };
        mappl = pkgs.callPackage ./mappl.nix {
          buildDunePackage = pkgs.ocamlPackages.buildDunePackage;
          ocamlPackages = pkgs.ocamlPackages;
        };
        dice = pkgs.callPackage ./dice.nix { };
        perpl = pkgs.callPackage ./perpl.nix { };
        fggs = pkgs.callPackage ./fggs.nix { };
        artifact-evaluation = (with pkgs; [
              coreutils-full
              bash
              zsh
              fish
              tree
              gnumake
              nano
              vim
              mappl
              dice
              hyperfine
              perl
              (python3.withPackages (ps: with ps; [
                pyro-ppl
                matplotlib
              ]))
              fggs
              perpl
            ]);
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = artifact-evaluation;
        };
      }
    );
}
