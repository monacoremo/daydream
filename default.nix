{ pkgs }:

rec {
  appName = "daydream";

  settings =
    pkgs.callPackage deploy/settings.nix
      { inherit appName; };

  webapp =
    pkgs.callPackage deploy/webapp.nix
    {
      inherit settings events dbLocal dbLocalSettings deployLocal
      checkedShellScript postgrestToElm;
    };

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

  dbLocal =
    pkgs.callPackage deploy/db.nix
      { inherit checkedShellScript postgresql md2sql; settings = dbLocalSettings; };

  dbLocalSettings =
    rec {
      binPrefix = "local-";
      dbDir = "$LOCAL_DB_DIR";
      dbHost = dbDir;
      dbSuperuser = "postgres";
      dbName = "postgres";
      dbSuperuserPassword = "postgres";
      dbSetupHost = "${dbDir}/setup";
      dbSrc = settings.dbSrc;
      dbSetupURI = "postgres:///postgres?host=${dbSetupHost}&user=postgres&password=postgres";
      dbApiserverPassword = "localpw";
      dbApiserverURI = "postgres:///postgres?host=${dbHost}&user=authenticator&password=${dbApiserverPassword}";
    };

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
