{
  description = "plutus-template-repo";

  inputs = {
    nixpkgs = {
      follows = "haskell-nix/nixpkgs";
    };
    std = {
      url = "github:divnix/std";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs = {
        hackage.follows = "hackage-nix";
      };
    };
    hackage-nix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    plutus = {
      url = "github:input-output-hk/plutus";
      flake = false;
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages/repo";
      flake = false;
    };
    sphinxcontrib-haddock = {
      url = "github:michaelpj/sphinxcontrib-haddock";
      flake = false;
    };
    gitignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-language-server = {
      # Pinned to a release
      url = "github:haskell/haskell-language-server?ref=1.9.0.0";
      flake = false;
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix?ref=86421fdd89b3af43fa716ccd07638f96c6ecd1e4";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:

    inputs.std.growOn
      {

        inherit inputs;
        cellsFrom = ./nix/cells;
        cellBlocks = [
          (inputs.std.devshells "devshells")
          (inputs.std.installables "packages")
          (inputs.std.functions "library")
          # (inputs.std.installables "ciJobs")
        ];
      }
      {
        devShells = inputs.std.harvest inputs.self [ "local" "devshells" ];

        packages = inputs.std.harvest inputs.self [ "local" "packages" ];
      };
  # }
  # {
  #   hydraJobs = inputs.std.harvest inputs.self [ "automation" "ciJobs" ];
  # };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
  };
}
