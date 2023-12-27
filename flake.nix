{
  description = "hasql-effectful";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcVersion = "ghc946";
        frameworks = pkgs.darwin.apple_sdk.frameworks;
        # haskellPackages = pkgs.haskell.packages."${ghcVersion}";
      in
      {
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              fourmolu.enable = true;
            };
            settings = {
              ormolu.defaultExtensions = [ "ImportQualifiedPost" "TypeApplications" ];
            };
          };
        };
        devShell = nixpkgs.legacyPackages.${system}.mkShell {
          buildInputs = [
            frameworks.Cocoa
          ];
          nativeBuildInputs = [
            pkgs.zlib
            pkgs.xz
            pkgs.just
            pkgs.cabal-install
            pkgs.haskell.packages."${ghcVersion}".haskell-language-server
            pkgs.haskell.compiler."${ghcVersion}"
            pkgs.postgresql_16
            pkgs.process-compose
            pkgs.jq
          ];
          shellHook = ''
            ${self.checks.${system}.pre-commit-check.shellHook}
            export LANG=en_US.UTF-8
            export PGHOST="$PWD/db"
            export PGDATA="$PGHOST/db"
            export PGLOG=$PGHOST/postgres.log
            export PGDATABASE=hasql-effectful
            export PG_CONNECTION_STRING=postgresql://$(jq -rn --arg x $PGHOST '$x|@uri')/$PGDATABASE
            export PC_PORT_NUM=9999

            mkdir -p $PGHOST
            mkdir -p .dev 

            if [ ! -d $PGDATA ]; then
              initdb --auth=trust --no-locale --encoding=UTF8 
            fi
          '';

        };
      }
    );
}
