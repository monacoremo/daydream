let
  pkgs =
    import ./nixpkgs.nix { config = { packageOverrides = packageOverrides; }; };

  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = newHaskellPkgs: oldHaskellPkgs: rec {
        elm-syntax = newHaskellPkgs.callPackage ./deps/elm-syntax.nix {};
      };
    };
  };
in
{ postgrestToElm = pkgs.haskellPackages.callCabal2nix "postgrest-to-elm" ./. {};
}
