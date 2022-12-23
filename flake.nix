{
  description = "Flake to build the haskell-src package 'named-text' and dependencies";

  nixConfig.bash-prompt-suffix = "named-text.env} ";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/22.05"; };
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sayable = {
      url = "/home/kquick/Projects/sayable/sayable-2211";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
    };
  };

  outputs = { self, levers, nixpkgs, sayable }:
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
      defaultPackage = levers.eachSystem (s:
        self.packages.${s}.named-text.default);
      devShell = levers.eachSystem (s:
        let pkgs = import nixpkgs { system=s; };
        in shellWith pkgs shellPkgs
          (defaultPackage.${s}.env.overrideAttrs (a:
            {
              # Set envvars here
            }
          )));

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
            in pkgs.lib.genAttrs names (oneshell s)
          ) ;

      packages = levers.eachSystem (system:
        let
          mkHaskell = levers.mkHaskellPkg {
            inherit nixpkgs system;
            ghcver = [ "ghc8107" ];
            };
          pkgs = import nixpkgs { inherit system; };
          haskellAdj = drv:
            with (pkgs.haskell).lib;
            dontHaddock (dontCheck (dontBenchmark (disableLibraryProfiling (disableExecutableProfiling drv))));
        in rec {
          ghc = pkgs.haskell.compiler.ghc8107;
          named-text = mkHaskell "named-text" self {
            inherit sayable;
            adjustDrv = args:
              drv:
                haskellAdj drv;
            };
          named-text_tests = mkHaskell "named-text_tests" self {
            inherit sayable;
            adjustDrv = args:
              drv:
                pkgs.haskell.lib.doBenchmark (pkgs.haskell.lib.doCheck (haskellAdj drv));
            };
        });
    };
}