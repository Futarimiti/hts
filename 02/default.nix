{ haskell }:
haskell.packages.ghc9101.developPackage {
  root = ./.;
  source-overrides = {
    hts-utils = ../hts-utils;
  };
}
