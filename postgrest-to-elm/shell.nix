let
  pkgs =
    import ./nixpkgs.nix {};

  project =
    (import ./default.nix).postgrestToElm;
in
pkgs.stdenv.mkDerivation {
  name = "postgrest-to-elm-env";

  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.ghc
  ];
}
