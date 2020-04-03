{ settings
, elmPackages
, entr
, events
, dbLocal
, dbLocalSettings
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
        ${elmPackages.elm-format}/bin/elm-format "$workdir/src/Api" --yes

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
        dbdir="$(realpath "${settings.webappDir}"/db)"

        mkdir -p "$dbdir"

        export LOCAL_DB_DIR="$dbdir"

        ${dbLocal.setup} > /dev/null
        ${dbLocal.startDaemon} > /dev/null

        ${postgrestToElm}/bin/postgrest-to-elm \
          --db-uri "${dbLocalSettings.dbApiserverURI}" --role webuser --schema api \
          --target-directory "$targetdir"

        ${dbLocal.stopDaemon} > /dev/null
      '';

  watch =
    checkedShellScript "${binPrefix}watch"
      ''
        find "${settings.webappSrc}" | ${entr}/bin/entr -d ${build}
      '';
}
