{
  description = "My solutions to HackThisSite programming missions, mostly in Haskell";

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
        getMissionPackages =
          with builtins;
          with pkgs.lib;
          let
            stringIsNat = str: match "^[0-9]+$" str != null;
          in
          dir:
          readDir dir
          |> filterAttrs (name: state: state == "directory" && stringIsNat name)
          |> mapAttrs (name: _: pkgs.callPackage (path.append dir name) { });
      in
      {
        packages = getMissionPackages ./.;
        devShells = {
          default = haskellPackages.shellFor {
            packages = _: [ ];
            nativeBuildInputs = [
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server
              haskellPackages.hpack
              pkgs.cairo
              pkgs.cookiecutter
              pkgs.expat
              pkgs.openssl
              pkgs.pkg-config
              pkgs.xorg.libXdmcp
              pkgs.zlib
            ];
            DYLD_LIBRARY_PATH = with pkgs; "${lib.getLib openssl}/lib";
          };
        };
      }
    );
}
