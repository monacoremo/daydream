{ elmPackages, entr, events, db, deployLocal, checkedShellScript, postgrestToElm }:

rec {
  build =
    checkedShellScript.writeBin "fullstack-webapp-build"
      ''
        srcdir="$FULLSTACK_WEBAPP_SRC"
        workdir="$FULLSTACK_WEBAPP_DIR"
        webroot="$FULLSTACK_WEBAPP_WEBROOT"

        mkdir -p "$workdir" "$webroot"
        ln -sf "$srcdir"/{elm.json,src} "$workdir"
        ln -sf "$srcdir"/index.html "$webroot"/index.html

        ${events}/bin/elmgen --target-directory "$workdir/src"
        ${generatePostgrestBindings.bin}

        cd "$workdir"

        ${elmPackages.elm}/bin/elm make src/Main.elm \
          --output "$workdir"/app.js --debug

        cat "$srcdir"/init.js >> "$workdir"/app.js
        cp "$workdir"/app.js "$webroot"/app.js
      '';

  generatePostgrestBindings =
    checkedShellScript.script "fullstack-webapp-postgrest-gen"
      ''
        targetdir="$FULLSTACK_WEBAPP_DIR"/src

        tmpdir="$(mktemp -d)"
        trap 'rm -rf $tmpdir' exit

        # shellcheck disable=SC1090
        source "$(${deployLocal.mkEnv}/bin/fullstack-local-mkenv . "$tmpdir")"

        ${db.setup}/bin/fullstack-db-setup
        ${db.startDaemon}/bin/fullstack-db-daemon-start

        ${postgrestToElm}/bin/postgrest-to-elm \
          --db-uri "$FULLSTACK_DB_APISERVER_URI" --role webuser --schema api \
          --target-directory "$targetdir"

        ${db.stopDaemon}/bin/fullstack-db-daemon-stop
      '';

  watch =
    checkedShellScript.writeBin "fullstack-webapp-watch"
      ''
        find "$FULLSTACK_WEBAPP_SRC" | \
          ${entr}/bin/entr -d ${build}/bin/fullstack-webapp-build
      '';
}
