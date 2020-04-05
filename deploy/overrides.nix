pkgs:

{
  haskellPackages =
    pkgs.haskellPackages.override {
      overrides =
        newPkgs: oldPkgs: rec {
          servant-to-elm =
            newPkgs.callPackage haskell/servant-to-elm.nix {};

          haskell-to-elm =
            newPkgs.callPackage haskell/haskell-to-elm.nix
              { inherit elm-syntax; };

          elm-syntax =
            newPkgs.callPackage haskell/elm-syntax.nix {};

          postgrest =
            newPkgs.callPackage haskell/postgrest.nix
              { inherit hasql-pool configurator-pg; };

          hasql-pool =
            newPkgs.callPackage haskell/hasql-pool.nix {};

          configurator-pg =
            newPkgs.callPackage haskell/configurator-pg.nix {};
        };
    };
}
