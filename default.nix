let
  nixpkgsVersion =
    import deploy/nixpkgs-version.nix;

  pinnedPkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  packageOverrides =
    import deploy/overrides.nix;

  pkgs =
    import pinnedPkgs { config = { inherit packageOverrides; }; };
in
rec {
  inherit pkgs;

  appName = "daydream";

  settings =
    pkgs.callPackage deploy/settings.nix
      { inherit appName; };

  docs =
    pkgs.callPackage deploy/docs.nix
      { inherit settings checkedShellScript python; };

  ingress =
    pkgs.callPackage deploy/ingress.nix
      { inherit settings checkedShellScript; };

  api =
    pkgs.callPackage deploy/api.nix
      { inherit settings checkedShellScript; };

  db =
    pkgs.callPackage deploy/db.nix
      { inherit settings checkedShellScript md2sql; };

  webapp =
    pkgs.callPackage deploy/webapp.nix
      {
        inherit settings events dbLocal dbLocalSettings deployLocal
          checkedShellScript postgrestToElm
          ;
      };

  dbLocal =
    pkgs.callPackage deploy/db.nix
      { inherit checkedShellScript md2sql; settings = dbLocalSettings; };

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
      dbSetupURI =
        "postgres:///${dbName}?host=${dbSetupHost}&user=${dbSuperuser}"
        + "&password=${dbSuperuserPassword}";
      dbApiserver = "authenticator";
      dbApiserverPassword = "postgres";
      dbApiserverURI =
        "postgres:///${dbName}?host=${dbHost}"
        + "&user=${dbApiserver}&password=${dbApiserverPassword}";
    };

  deployLocal =
    pkgs.callPackage deploy/local.nix
      { inherit settings checkedShellScript python db api ingress webapp docs logmux; };

  python =
    pkgs.python38;

  tests =
    pkgs.callPackage deploy/tests.nix
      { inherit settings checkedShellScript python deployLocal; };

  checkedShellScript =
    pkgs.callPackage deploy/checked-shell-script.nix {};

  nixpkgsUpdate =
    pkgs.callPackage deploy/nixpkgs-update.nix
      { inherit checkedShellScript; };

  logmux =
    python.withPackages (
      ps:
        [ ps.callPackage deploy/python-packages/logmux.nix {} ]
    );

  md2sql =
    deploy/utils/md2sql.sed;

  sql2md =
    deploy/utils/sql2md.sed;

  events =
    (pkgs.callPackage events/default.nix {}).events;

  postgrestToElm =
    (pkgs.callPackage deploy/postgrest-to-elm/default.nix {}).postgrestToElm;

  autoformat =
    pkgs.callPackage deploy/autoformat.nix
      { inherit md2sql sql2md checkedShellScript settings; };
}
