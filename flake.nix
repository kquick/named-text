{
  description = "Flake to build the haskell-src package 'named-text' and dependencies";

  nixConfig.bash-prompt-suffix = "named-text.env} ";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/23.05"; };
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
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
    };
  };

  outputs = { self, levers, nixpkgs
            , parameterized-utils-src
            , tasty-checklist
            , sayable }:
    let
      shellWith = pkgs: adds: drv: drv.overrideAttrs(old:
        { buildInputs = old.buildInputs ++ adds pkgs; });
      # Add additional packages useful for a development shell, generally
      # representing test packages or non-propagated build dependencies of
      # various sub-packages.
      shellPkgs = pkgs: [
        # pkgs.haskell.compiler.integer-simple.ghc8107
        # pkgs.haskell.packages.ghc8107.profiteur
        pkgs.cabal-install
      ];
    in rec {
      devShells =
        let oneshell = s: n:
              let pkgs = import nixpkgs { system=s; };
              in levers.variedTargets
                { ghcver = levers.validGHCVersions pkgs.haskell.compiler; }
                ( { ghcver, ... } @ vargs:
                  shellWith pkgs shellPkgs
                    (self.packages.${s}.${n}.${ghcver}.env.overrideAttrs (a:
                      {
                        # Set envvars here
                      }
                    )));
        in levers.eachSystem
          (s:
            let pkgs = import nixpkgs { system=s; };
                names = builtins.attrNames (self.packages.${s});
                outs = builtins.removeAttrs
                  (pkgs.lib.genAttrs names (oneshell s))
                  [ "ghc" ];
                shells = pkgs.lib.attrsets.mapAttrs (n: v: v.default) outs;
            in shells
          ) ;

      packages = levers.eachSystem (system:
        let
          mkHaskell = levers.mkHaskellPkg {
            inherit nixpkgs system;
            # ghcver = [ "ghc8107" ];
            };
          pkgs = import nixpkgs { inherit system; };
          haskellAdj = drv:
            with (pkgs.haskell).lib;
            dontHaddock (
              dontCheck (
                dontBenchmark (
                  # disableLibraryProfiling (
                  #   disableExecutableProfiling
                      drv)
                # )
              )
            );
        in rec {
          ghc = pkgs.haskell.compiler.ghc8107;
          default = named-text;
          TESTS =
            builtins.derivation
            {
            name = "all_named-text_flake_tests";
            inherit system;
            builder = "${pkgs.bash}/bin/bash";
            args = [ "-c" "echo OK > $out" ];
            buildInputs = [ named-text_tests ];
          };
          DOC =
            builtins.derivation
            {
            name = "all_named-text_flake_doc";
            inherit system;
            builder = "${pkgs.bash}/bin/bash";
            args = [ "-c" "echo OK > $out" ];
            buildInputs = [ named-text_doc ];
          };
          named-text = mkHaskell "named-text" self {
            inherit sayable;
            adjustDrv = args: haskellAdj;
            };
          named-text_tests = mkHaskell "named-text_tests" self {
            inherit parameterized-utils;
            inherit sayable;
            inherit tasty-checklist;
            adjustDrv = args:
              drv:
              pkgs.haskell.lib.doBenchmark
                (pkgs.haskell.lib.doCheck
                  (haskellAdj drv)
                );
          };
          named-text_doc = mkHaskell "named-text_doc" self {
            inherit sayable;
            adjustDrv = args:
              drv:
              pkgs.haskell.lib.doHaddock (haskellAdj drv);
            };
          parameterized-utils = mkHaskell "parameterized-utils"
            parameterized-utils-src {};
        });
    };
}
