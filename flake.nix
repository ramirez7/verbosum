{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    git-hooks-nix.inputs.nixpkgs.follows = "nixpkgs";
    git-hooks-nix.url = "github:cachix/git-hooks.nix";
    all-cabal-hashes.url = "github:commercialhaskell/all-cabal-hashes/hackage";
    all-cabal-hashes.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ 
        inputs.haskell-flake.flakeModule
        inputs.git-hooks-nix.flakeModule 
      ];
      perSystem = { self', inputs', pkgs, system, config, ... }: {
        pre-commit.settings.hooks = {
          cabal-gild.enable = true;
        };
        haskellProjects.default = {
          projectFlakeName = "haskell-multi-nix";
          basePackages = pkgs.haskellPackages.override {
            all-cabal-hashes = inputs.all-cabal-hashes;
          };

          # Want to override dependencies?
          # See https://haskell.flake.page/dependency
          packages = {
            claude.source = "1.4.0";
          };
          devShell = with pkgs; {
            mkShellArgs = {
              shellHook = ''
                export SHELL_HOOK_WORKED=1
              '' + config.pre-commit.installationScript;
              nativeBuildInputs = with pkgs;
                [ ghciwatch just haskellPackages.cabal-gild];
            };
            hoogle = false;
          };
          settings = {
            # The `claude` test suite needs an ANTHROPIC_KEY
            # https://github.com/MercuryTechnologies/claude/issues/4
            claude.check = false;
          };
        };

        # Minimal closure: Haskell libraries statically linked into the binary,
        # stripping lib outputs and doc references. 
        packages.verbosum-minimal = pkgs.haskell.lib.overrideCabal
          self'.packages.verbosum
          (_: {
            enableSharedExecutables = false;
            isLibrary = false;
            postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
          });
      };
    };
}
