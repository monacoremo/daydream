{ pkgs }:

rec {
  appName = "daydream";

  settings =
    pkgs.callPackage deploy/settings.nix
      { inherit appName; };

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

  deployLocal =
    pkgs.callPackage deploy/local.nix
      { inherit settings checkedShellScript python db api ingress webapp docs logmux; };

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

  postgrest =
    pkgs.callPackage deploy/postgrest.nix {};

  tests =
    pkgs.callPackage deploy/tests.nix
      { inherit settings checkedShellScript python deployLocal; };

  checkedShellScript =
    pkgs.callPackage deploy/checked-shell-script.nix {};

  nixpkgsUpdate =
    pkgs.callPackage deploy/nixpkgs-update.nix
      { inherit checkedShellScript; };

  logmux =
    deploy/utils/logmux.py;

  md2sql =
    deploy/utils/md2sql.sed;

  geckodriver =
    pkgs.geckodriver;

  firefox =
    pkgs.firefox;

  events =
    (pkgs.callPackage events/default.nix {}).events;

  postgrestToElm =
    (pkgs.callPackage deploy/postgrest-to-elm/default.nix {}).postgrestToElm;

}
