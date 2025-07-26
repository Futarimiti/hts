{
  description = "My solutions to HackThisSite programming missions in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem
      (
        system:
        let
          pkgs =
            nixpkgs.legacyPackages.${system};
          haskellPackages =
            pkgs.haskell.packages.ghc9101;
          findPackages =
            with builtins;
            with pkgs.lib;
            let
              stringIsNat =
                str:
                match "^[0-9]+$" str
                != null;
            in
            dir:
            readDir dir
            |>
              filterAttrs
                (
                  name: state:
                  state
                  == "directory"
                  && stringIsNat name
                )
            |>
              mapAttrs
                (
                  name: _:
                  haskellPackages.developPackage
                    {
                      root = path.append dir name;
                    }
                );
        in
        {
          packages = findPackages ./.;
          devShells = {
            default =
              haskellPackages.shellFor
                {
                  packages =
                    _:
                    [ ];
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
