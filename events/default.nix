let
  pkgs =
    import ./nixpkgs.nix { config = { packageOverrides = packageOverrides; }; };

  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = newHaskellPkgs: oldHaskellPkgs: rec {
        servant-to-elm = newHaskellPkgs.callPackage ./deps/servant-to-elm.nix {};
        haskell-to-elm = newHaskellPkgs.callPackage ./deps/haskell-to-elm.nix {inherit elm-syntax;};
        elm-syntax = newHaskellPkgs.callPackage ./deps/elm-syntax.nix {};
      };
    };
  };
in
{ events = pkgs.haskellPackages.callCabal2nix "events" ./. {};
}
