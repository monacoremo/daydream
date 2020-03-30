let
  pkgs =
    import ./nixpkgs.nix {};

  project =
    (import ./default.nix).events;
in
pkgs.stdenv.mkDerivation {
  name = "events-env";

  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.ghc
    pkgs.openssl.dev
  ];
}
