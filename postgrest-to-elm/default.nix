let
  pkgs =
    import ./nixpkgs.nix { config = { packageOverrides = packageOverrides; }; };

  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = newHaskellPkgs: oldHaskellPkgs: rec {
        elm-syntax = newHaskellPkgs.callPackage ./deps/elm-syntax.nix {};
        postgrest = newHaskellPkgs.callPackage ./deps/postgrest.nix { inherit hasql-pool configurator-pg; };
        hasql-pool = newHaskellPkgs.callPackage ./deps/hasql-pool.nix {};
        configurator-pg = newHaskellPkgs.callPackage ./deps/configurator-pg.nix {};
      };
    };
  };
in
{ postgrestToElm = pkgs.haskellPackages.callCabal2nix "postgrest-to-elm" ./. {};
}
