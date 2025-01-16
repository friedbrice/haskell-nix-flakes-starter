{
  description = "Haskell development tool chain";

  # every flake has some inputs.
  # each input is a snapshot of some software repository, or a reference to a directory on the file system.
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  # every flake takes their inputs and computes some outputs.
  # `outputs` is a function that takes an attribute set (the nix word for _record_) as argument.
  # the attributes are `self` and whatever you declared in `inputs`.
  outputs = { self, nixpkgs, flake-utils }:
    # `eachDefaultSystem` allows us to avoid hard-coding our platform (e.g. 'aarch64-darwin')
    # this way, this same flake can be used on any platform.
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          all-packages = import nixpkgs {
            inherit system;
            config.allowBroken = true;
            overlays = [];
          };

          project-ghc-version = "ghc98";

          haskell-packages = all-packages.haskell.packages."${project-ghc-version}";

          # project-ghc = all-packages.haskell.compiler."${project-ghc-version}"
          project-ghc = haskell-packages.ghcWithPackages (ps: [
            ps.digest
            ps.hmatrix
            ps.silently
          ]);

        in
        {
          devShells.default = all-packages.mkShell {
            buildInputs = [
              # Generic utilities
              all-packages.jq

              # Haskell development tools
              project-ghc
              haskell-packages.haskell-language-server
              haskell-packages.cabal-install
              haskell-packages.hlint
              haskell-packages.fourmolu
              haskell-packages.cabal-fmt

              # C libraries
              all-packages.blas # needed by hmatrix
              all-packages.lapack # needed by hmatrix
              all-packages.zlib # needed by digest
            ];

            shellHook = ''
              rm -rf .dev
              mkdir -p .dev/bin

              ln -sf `which ghc` .dev/bin/ghc
              ln -sf `which haskell-language-server` .dev/bin/hls
              ln -sf `which cabal` .dev/bin/cabal
              ln -sf `which hlint` .dev/bin/hlint
              ln -sf `which fourmolu` .dev/bin/fourmolu
              ln -sf `which cabal-fmt` .dev/bin/cabal-fmt

              export PATH="$PWD/.dev/bin:$PATH"

              if [ -f .vscode/settings.json ]; then
                mv .vscode/settings.json .vscode/temp0.json

                jq_program=".\"haskell.serverEnvironment\".PATH |= \"$PATH\""
                jq "$jq_program" .vscode/settings-template.json >.vscode/temp1.json

                jq_program=".\"haskell.serverEnvironment\".NIX_CFLAGS_COMPILE |= \"$NIX_CFLAGS_COMPILE\""
                jq "$jq_program" .vscode/temp1.json >.vscode/temp2.json

                jq_program=".\"haskell.serverEnvironment\".buildInputs |= \"$buildInputs\""
                jq "$jq_program" .vscode/temp2.json >.vscode/temp3.json

                jq_program=".\"haskell.serverEnvironment\".NIX_LDFLAGS |= \"$NIX_LDFLAGS\""
                jq "$jq_program" .vscode/temp3.json >.vscode/settings.json

                rm -f .vscode/temp*.json
              fi

              echo "Hello, Nix."
            '';
          };
        }
      );
}
