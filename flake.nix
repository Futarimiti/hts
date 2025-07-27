{
  description = "My solutions to HackThisSite programming missions in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc9101;
        xml-lens = pkgs.fetchFromGitHub {
          owner = "bellroy";
          repo = "xml-lens";
          rev = "41248a393579d27f3b0432714113f159412f028d";
          sha256 = "sha256-P7oYIDEgmo/eGoPDfULttXq4+fjlQGkJ8hI1t9hh3Hs=";
        };
        findPackages =
          with builtins;
          with pkgs.lib;
          let
            stringIsNat = str: match "^[0-9]+$" str != null;
          in
          dir:
          readDir dir
          |> filterAttrs (name: state: state == "directory" && stringIsNat name)
          |> mapAttrs (
            name: _:
            haskellPackages.developPackage {
              root = path.append dir name;
              source-overrides = { inherit xml-lens; };
            }
          );
      in
      {
        packages = findPackages ./.;
        devShells = {
          default = haskellPackages.shellFor {
            packages = _: [ ];
            nativeBuildInputs = [
              pkgs.zlib
              pkgs.pkg-config
              pkgs.cookiecutter
              pkgs.cairo
              pkgs.expat
              pkgs.xorg.libXdmcp
              haskellPackages.cabal-install
              haskellPackages.hpack
              haskellPackages.haskell-language-server
            ];
          };
        };
      }
    );
}
