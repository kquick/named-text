{
  description = "Flake to build the haskell-src package 'named-text' and dependencies";

  nixConfig.bash-prompt-suffix = "named-text.env} ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sayable = {
      url = "github:kquick/sayable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
    };
    parameterized-utils-src = {
      url = "github:galoisinc/parameterized-utils";
      flake = false;
    };
    tasty-checklist = {
      url = "github:kquick/tasty-checklist";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
    };
  };

  outputs = { self, levers, nixpkgs
            , parameterized-utils-src
            , tasty-checklist
            , sayable }:
    rec {
      devShells = levers.haskellShells
        { inherit nixpkgs;
          flake = self;
          defaultPkg = "named-text";
          # additionalPackages = pkgs.haskell.packages.ghc8107.profiteur
        };

      packages = levers.eachSystem (system:
        let
          mkHaskell = levers.mkHaskellPkg { inherit nixpkgs system; };
          pkgs = import nixpkgs { inherit system; };
        in rec {
          default = named-text;
          named-text = mkHaskell "named-text" self {
            inherit parameterized-utils;
            inherit sayable;
            inherit tasty-checklist;
          };
          parameterized-utils = mkHaskell "parameterized-utils"
            parameterized-utils-src {};
        });
    };
}
