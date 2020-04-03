{ pkgs }:

rec {
  appName = "daydream";

  postgresqlWithPerl =
    pkgs.postgresql_12.overrideAttrs (attrs: {
      configureFlags =
        attrs.configureFlags ++ [ "--with-perl" ];

      buildInputs =
        attrs.buildInputs ++ [
          (pkgs.perl.withPackages (ps: [ ps.EmailValid ]))
        ];
    });

  postgresql =
    postgresqlWithPerl.withPackages
      (ps: [
        ps.pgtap
      ]);

  python =
    pkgs.python38.withPackages
      (ps: [
          ps.pytest
          ps.requests
          ps.selenium
          ps.click
      ]);

  logmux =
    deploy/utils/logmux.py;

  md2sql =
    deploy/utils/md2sql.sed;

  nixpkgsUpdate =
    pkgs.callPackage deploy/nixpkgs-update.nix { inherit checkedShellScript; };

  geckodriver =
    pkgs.geckodriver;

  settings =
    pkgs.callPackage deploy/settings.nix { inherit appName; };

  events =
    (pkgs.callPackage events/default.nix {}).events;

  postgrestToElm =
    (pkgs.callPackage deploy/postgrest-to-elm/default.nix {}).postgrestToElm;

  postgrest =
    pkgs.callPackage deploy/postgrest.nix {};

  webapp =
    pkgs.callPackage deploy/webapp.nix
      { inherit settings events db deployLocal checkedShellScript postgrestToElm; };

  docs =
    pkgs.callPackage deploy/docs.nix
      { inherit settings checkedShellScript; };

  ingress =
    pkgs.callPackage deploy/ingress.nix
      { inherit settings checkedShellScript; };

  api =
    pkgs.callPackage deploy/api.nix
      { inherit settings checkedShellScript postgrest; };

  db =
    pkgs.callPackage deploy/db.nix
      { inherit settings checkedShellScript postgresql md2sql; };

  checkedShellScript =
    pkgs.callPackage deploy/checked-shell-script.nix {};

  deployLocal =
    pkgs.callPackage deploy/local.nix
      { inherit settings checkedShellScript python db api ingress webapp docs logmux; };

  tests =
    pkgs.callPackage deploy/tests.nix
      { inherit settings checkedShellScript python deployLocal; };
}
