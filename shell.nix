with (import ./default.nix);
pkgs.mkShell {
  name = "${settings.binPrefix}env";

  buildInputs = [
    api.run.bin
    api.watch.bin
    db.run.bin
    db.watch.bin
    db.setup.bin
    db.startDaemon.bin
    db.stopDaemon.bin
    db.test.bin
    deployLocal.mkEnv.bin
    deployLocal.run.bin
    deployLocal.watch.bin
    ingress.run.bin
    webapp.build.bin
    webapp.watch.bin
    webapp.generatePostgrestBindings.bin
    webapp.test.bin
    tests.run.bin
    tests.watch.bin
    docs.build.bin
    docs.watch.bin
    nixpkgsUpdate.bin
    python
    autoformat.bin
    pkgs.curl
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-test
    pkgs.silver-searcher
    pkgs.cabal2nix
  ];

  shellHook = ''
    tmpdir="$(mktemp -d)"
    trap 'rm -rf $tmpdir' exit

    port="$(${randomfreeport})"
    source "$(${deployLocal.mkEnv} . "$tmpdir" "$port")"

    echo "${settings.appName} set up in ${settings.dir} and ${settings.URI}"

    alias ${settings.binPrefix}open="xdg-open ${settings.URI}"

    # psql variables for convenience
    export PGHOST="${settings.dbHost}"
    export PGDATABASE="${settings.dbName}"
    export PGUSER="${settings.dbSuperuser}"
    export PGPASSWORD="${settings.dbSuperuserPassword}"

    # disable line wrap in psql
    export PAGER="less -S"
  '';
}
