{ inputs, cell }:

{ compiler-nix-name ? cell.library.ghc-compiler-nix-name }:

let
  project = cell.library.haskell-nix.cabalProject' ({ pkgs, lib, ... }:
    let isCrossCompiling = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {

      inherit compiler-nix-name;

      # This is incredibly difficult to get right, almost everything goes wrong,
      # see https://github.com/input-output-hk/haskell.nix/issues/496
      src = cell.library.haskell-nix.haskellLib.cleanSourceWith {

        src = inputs.self.outPath;

        # Otherwise this depends on the name in the parent directory, which reduces caching, and is
        # particularly bad on Hercules, see https://github.com/hercules-ci/support/issues/40
        name = "plutus";
      };

      shell = {
        # We don't currently use this.
        withHoogle = false;

        # We would expect R to be pulled in automatically as it's a dependency of
        # plutus-core, but it appears it is not, so we need to be explicit about
        # the dependency on R here.  Adding it as a buildInput will ensure it's
        # added to the pkg-config env var.
        buildInputs = [ pkgs.R pkgs.libblst];
      };

      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
        "https://github.com/input-output-hk/plutus" = inputs.plutus;
      };
      sha256map = {
        "https://github.com/tweag/HaskellR"."411d15fe5027494123e326c838955eff1c8e7ec8" = "0jax08z81xbfs3xz7zkk7x83cmr487iglifmxri205mf5bcj8ycj"; # editorconfig-checker-disable-line
      };

      # TODO: move this into the cabal.project using the new conditional support?
      # Configuration settings needed for cabal configure to work when cross compiling.
      # We can't use `modules` for these as `modules` are only applied
      # after cabal has been configured.
      cabalProjectLocal = lib.optionalString isCrossCompiling ''
        -- When cross compiling we don't have a `ghc` package, so use
        -- the `plutus-ghc-stub` package instead.
        package plutus-tx-plugin
          flags: +use-ghc-stub

        -- Exclude tests that use `doctest`.  They will not work for
        -- cross compilation and `cabal` will not be able to make a plan.
        package prettyprinter-configurable
          tests: False
      '';

      modules = [

        # Cross compiling
        ({ pkgs, ... }: lib.mkIf isCrossCompiling {
          packages = { };
        })

        # Common
        ({ pkgs, config, ... }: {
          packages = { };
        })

        # -Werror for CI
        # Only enable on the newer compilers. We don't care about warnings on the old ones,
        # and sometimes it's hard to be warning free on all compilers, e.g. the unused
        # packages warning is bad in 8.10.7
        # (https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6130)
        (lib.mkIf (compiler-nix-name != "ghc8107") {
          packages = {
            # Werror everything.
            # This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
          };
        })

      ];
    });
in
project

