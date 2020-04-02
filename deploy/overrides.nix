pkgs:

rec {
  haskellPackages =
    pkgs.haskellPackages.override {
      overrides = newHaskellPkgs: oldHaskellPkgs: rec {
        servant-to-elm =
          newHaskellPkgs.callPackage haskell/servant-to-elm.nix {};

        haskell-to-elm =
          newHaskellPkgs.callPackage haskell/haskell-to-elm.nix
            { inherit elm-syntax; };

        elm-syntax =
          newHaskellPkgs.callPackage haskell/elm-syntax.nix {};

        postgrest =
          newHaskellPkgs.callPackage haskell/postgrest.nix
            { inherit hasql-pool configurator-pg; };

        hasql-pool =
          newHaskellPkgs.callPackage haskell/hasql-pool.nix {};

        configurator-pg =
          newHaskellPkgs.callPackage haskell/configurator-pg.nix {};
      };
    };

  postgresql_12 =
    pkgs.postgresql_12.overrideAttrs (attrs: {
      configureFlags =
        attrs.configureFlags ++ [ "--with-perl" ];

      buildInputs =
        attrs.buildInputs ++ [
          (pkgs.perl.withPackages (ps: [ ps.EmailValid ]))
        ];
    });
}
