{ settings
, elmPackages
, entr
, events
, db
, deployLocal
, checkedShellScript
, postgrestToElm
}:

let
  binPrefix =
    "${settings.binPrefix}webapp-";
in
rec {
  build =
    checkedShellScript "${binPrefix}build"
      ''
        srcdir="${settings.webappSrc}"
        workdir="${settings.webappDir}"
        webroot="${settings.webappWebroot}"

        mkdir -p "$workdir" "$webroot"
        ln -sf "$srcdir"/{elm.json,src} "$workdir"
        ln -sf "$srcdir"/index.html "$webroot"/index.html

        echo "Generating Elm bindings..."
        ${events}/bin/elmgen --target-directory "$workdir/src"
        ${generatePostgrestBindings}

        echo "Building..."
        cd "$workdir"

        ${elmPackages.elm}/bin/elm make src/Main.elm \
          --output "$workdir"/app.js --debug

        cat "$srcdir"/init.js >> "$workdir"/app.js
        cp "$workdir"/app.js "$webroot"/app.js
      '';

  generatePostgrestBindings =
    checkedShellScript "${binPrefix}postgrest-gen"
      ''
        targetdir="${settings.webappDir}"/src

        tmpdir="$(mktemp -d)"
        trap 'rm -rf $tmpdir' exit

        # shellcheck disable=SC1090
        source "$(${deployLocal.mkEnv} . "$tmpdir")"

        ${db.setup} > /dev/null
        ${db.startDaemon} > /dev/null

        ${postgrestToElm}/bin/postgrest-to-elm \
          --db-uri "${settings.dbApiserverURI}" --role webuser --schema api \
          --target-directory "$targetdir"

        ${db.stopDaemon} > /dev/null
      '';

  watch =
    checkedShellScript "${binPrefix}watch"
      ''
        find "${settings.webappSrc}" | ${entr}/bin/entr -d ${build}
      '';
}
