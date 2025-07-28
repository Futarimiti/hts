{ haskell, fetchFromGitHub }:
haskell.packages.ghc9101.developPackage {
  root = ./.;
  source-overrides = {
    xml-lens = fetchFromGitHub {
      owner = "bellroy";
      repo = "xml-lens";
      rev = "41248a393579d27f3b0432714113f159412f028d";
      sha256 = "sha256-P7oYIDEgmo/eGoPDfULttXq4+fjlQGkJ8hI1t9hh3Hs=";
    };
    hts-utils = ../hts-utils;
  };
}
