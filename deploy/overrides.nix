pkgs:

{
  haskellPackages =
    pkgs.haskellPackages.override {
      overrides =
        newPkgs: oldPkgs: rec {
          servant-to-elm =
            newPkgs.callPackage haskell-packages/servant-to-elm.nix {};

          haskell-to-elm =
            newPkgs.callPackage haskell-packages/haskell-to-elm.nix
              { inherit elm-syntax; };

          elm-syntax =
            newPkgs.callPackage haskell-packages/elm-syntax.nix {};

          postgrest =
            newPkgs.callPackage haskell-packages/postgrest.nix
              { inherit hasql-pool configurator-pg; };

          hasql-pool =
            newPkgs.callPackage haskell-packages/hasql-pool.nix {};

          configurator-pg =
            newPkgs.callPackage haskell-packages/configurator-pg.nix {};
        };
    };
}
