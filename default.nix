let
  nixpkgsVersion =
    import pkgs/nixpkgs-version.nix;

  pinnedPkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  pkgs =
    import pinnedPkgs { overlays = [ (import pkgs/overlay.nix) ]; };
in
rec {
  inherit pkgs;

  appName = "daydream";

  settings =
    pkgs.callPackage pkgs/settings.nix
      { inherit appName; };

  docs =
    pkgs.callPackage pkgs/docs.nix
      { inherit settings checkedShellScript python module; };

  ingress =
    pkgs.callPackage pkgs/ingress.nix
      { inherit settings checkedShellScript module; };

  api =
    pkgs.callPackage pkgs/api.nix
      { inherit settings checkedShellScript module; };

  db =
    pkgs.callPackage pkgs/db.nix
      { inherit settings checkedShellScript md2sql module; };

  webapp =
    pkgs.callPackage pkgs/webapp.nix
      {
        inherit settings events dbLocal dbLocalSettings deployLocal
          checkedShellScript postgrestToElm module
          ;
      };

  dbLocal =
    pkgs.callPackage pkgs/db.nix
      { inherit checkedShellScript md2sql module; settings = dbLocalSettings; };

  dbLocalSettings =
    rec {
      binPrefix = "local-";
      dbDir = "$LOCAL_DB_DIR";
      dbHost = dbDir;
      dbSuperuser = "postgres";
      dbName = "postgres";
      dbSuperuserPassword = "postgres";
      dbSuperuserURI =
        "postgres:///${dbName}?host=${dbHost}&user=${dbSuperuser}"
        + "&password=${dbSuperuserPassword}";
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
    pkgs.callPackage pkgs/deploy-local.nix
      {
        inherit settings checkedShellScript python db api ingress webapp docs
          randomfreeport logmux module
          ;
      };

  python =
    pkgs.python38;

  tests =
    pkgs.callPackage pkgs/tests.nix
      { inherit settings checkedShellScript python deployLocal module; };

  checkedShellScript =
    pkgs.callPackage pkgs/checked-shell-script.nix {};

  nixpkgsUpdate =
    pkgs.callPackage pkgs/nixpkgs-update.nix
      { inherit checkedShellScript; };

  logmux =
    python.pkgs.callPackage pkgs/python-packages/logmux.nix {};

  md2sql =
    pkgs/utils/md2sql.sed;

  sql2md =
    pkgs/utils/sql2md.sed;

  events =
    pkgs.callPackage events/default.nix {};

  postgrestToElm =
    pkgs.callPackage pkgs/postgrest-to-elm/default.nix {};

  autoformat =
    pkgs.callPackage pkgs/autoformat.nix
      { inherit md2sql sql2md checkedShellScript settings; };

  randomfreeport =
    checkedShellScript "randomfreeport"
      ''
        ${python}/bin/python ${pkgs/utils/randomfreeport.py}
      '';

  module =
    name: attrs:
      pkgs.buildEnv {
        inherit name;
        paths = pkgs.lib.catAttrs "bin" (pkgs.lib.attrValues attrs);
      } // attrs;
}
