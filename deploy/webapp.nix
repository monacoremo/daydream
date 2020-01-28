{ writeShellScriptBin, elmPackages, entr }:

rec {
  build =
    writeShellScriptBin "fullstack-webapp-build"
      ''
        set -e

        srcdir="$FULLSTACK_WEBAPP_SRC"
        workdir="$FULLSTACK_WEBAPP_DIR"
        webroot="$FULLSTACK_WEBAPP_WEBROOT"

        mkdir -p "$workdir" "$webroot"
        ln -sf "$srcdir"/{elm.json,src} "$workdir"
        ln -sf "$srcdir"/index.html "$webroot"/index.html

        cd "$workdir"

        ${elmPackages.elm}/bin/elm make src/Main.elm \
          --output "$workdir"/app.js --debug

        cat "$srcdir"/init.js >> "$workdir"/app.js
        cp "$workdir"/app.js "$webroot"/app.js
      '';

  watch =
    writeShellScriptBin "fullstack-webapp-watch"
      ''
        find "$FULLSTACK_WEBAPP_SRC" | \
          ${entr}/bin/entr -d ${build}/bin/fullstack-webapp-build
      '';
}
