let
  project = import ./default.nix;

  pkgs =
    project.pkgs;

  autoformat =
    project.checkedShellScript "autoformat"
      ''
        ${pkgs.pythonPackages.autopep8}/bin/autopep8 -ri \
          "${project.settings.sourceDir}"/tests

        ${pkgs.elmPackages.elm-format}/bin/elm-format --yes \
          "${project.settings.sourceDir}"/webapp/src

        find . -iname "*.hs" -exec ${pkgs.ormolu}/bin/ormolu --mode inplace {} +
      '';
in
project.pkgs.mkShell {
  name = "${project.settings.appName}-env";

  buildInputs = [
    project.api.run.bin
    project.api.watch.bin
    project.db.run.bin
    project.db.watch.bin
    project.db.setup.bin
    project.db.startDaemon.bin
    project.db.stopDaemon.bin
    project.db.test.bin
    project.deployLocal.mkEnv.bin
    project.deployLocal.run.bin
    project.deployLocal.watch.bin
    project.deployLocal.withTmpEnv.bin
    project.ingress.run.bin
    project.webapp.build.bin
    project.webapp.watch.bin
    project.webapp.generatePostgrestBindings.bin
    project.tests.run.bin
    project.tests.runWithTmpEnv.bin
    project.tests.watch.bin
    project.docs.build.bin
    project.docs.watch.bin
    project.nixpkgsUpdate.bin
    project.postgresql
    project.postgrest
    project.python
    autoformat.bin
    pkgs.bash
    pkgs.curl
    pkgs.entr
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.silver-searcher
    pkgs.cabal2nix
    pkgs.ormolu
  ];

  shellHook = ''
    tmpdir="$(mktemp -d)"
    source "$(${project.deployLocal.mkEnv} . "$tmpdir")"
    trap 'rm -rf $tmpdir' exit
    echo "Environment for ${project.settings.appName} set up in $tmpdir"

    # psql variables for convenience
    export PGHOST="${project.settings.dbHost}"
    export PGDATABASE="${project.settings.dbName}"
    export PGUSER="${project.settings.dbSuperuser}"
    export PGPASSWORD="${project.settings.dbSuperuserPassword}"

    # disable line wrap in psql
    export PAGER="less -S"
  '';
}
