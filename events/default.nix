let
  pkgs =
    import ./nixpkgs.nix { config = { packageOverrides = packageOverrides; }; };

  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = newHaskellPkgs: oldHaskellPkgs: rec {
        servant-to-elm = newHaskellPkgs.callPackage ./deps/servant-to-elm.nix {};
        haskell-to-elm = newHaskellPkgs.callPackage ./deps/haskell-to-elm.nix {inherit elm-syntax;};
        elm-syntax = newHaskellPkgs.callPackage ./deps/elm-syntax.nix {};
        #generics-sop = newHaskellPkgs.callPackage ./deps/generics-sop.nix {inherit sop-core template-haskell;};
        #sop-core = newHaskellPkgs.callPackage ./deps/sop-core.nix {};
        #template-haskell = newHaskellPkgs.callPackage ./deps/template-haskell.nix {};
      };
    };
  };
in
{ events = pkgs.haskellPackages.callCabal2nix "events" ./. {};
}
