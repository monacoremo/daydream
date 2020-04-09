{ settings
, elmPackages
, entr
, events
, dbLocal
, dbLocalSettings
, deployLocal
, checkedShellScript
, postgrestToElm
, writeText
, silver-searcher
, module
}:
let
  binPrefix =
    "${settings.binPrefix}webapp-";

  index =
    writeText "index.html"
      ''
        <!doctype html>
        <meta charset="utf-8">
        <title>${settings.appName}</title>
        <link href="https://fonts.googleapis.com/css?family=Roboto&display=swap" rel="stylesheet">
        <script src="/app/app.js"></script>
      '';

  init =
    writeText "init.js"
      ''
        window.addEventListener('load', function() {
            const app = Elm.Main.init();
        });
      '';
in
module "webapp"
rec {
  build =
    checkedShellScript "${binPrefix}build"
      ''
        mkdir -p "${settings.webappDir}" "${settings.webappWebroot}"
        ln -sf "${settings.webappSrc}"/{elm.json,src} "${settings.webappDir}"
        ln -sf "${index}" "${settings.webappWebroot}"/index.html

        # force Elm to cache modules in the working directory
        #export HOME="${settings.webappDir}"

        echo "Generating Elm bindings..."
        ${events}/bin/elmgen --target-directory "${settings.webappDir}/src"
        ${generatePostgrestBindings}
        ${elmPackages.elm-format}/bin/elm-format "${settings.webappDir}/src/Generated" --yes

        echo "Building..."
        cd "${settings.webappDir}"
        ${elmPackages.elm}/bin/elm make src/Main.elm \
          --output "${settings.webappDir}"/app.js --debug

        cat "${init}" >> "${settings.webappDir}"/app.js
        cp "${settings.webappDir}"/app.js "${settings.webappWebroot}"/app.js
      '';

  generatePostgrestBindings =
    checkedShellScript "${binPrefix}postgrest-gen"
      ''
        gendir="$(realpath "${settings.webappDir}"/gen)"
        mkdir -p "$gendir"
        export LOCAL_DB_DIR="$gendir"

        ${dbLocal.setup} > /dev/null

        trap "${dbLocal.stopDaemon}" exit

        ${dbLocal.startDaemon} > /dev/null

        ${postgrestToElm}/bin/postgrest-to-elm \
          --db-uri "${dbLocalSettings.dbApiserverURI}" --role webuser --schema api \
          --target-directory "${settings.webappDir}/src"

        ${dbLocal.stopDaemon} > /dev/null

        trap - exit
      '';

  watch =
    checkedShellScript "${binPrefix}watch"
      ''
        while true; do
          ${silver-searcher}/bin/ag -l . "${settings.sourceDir}" | \
            ${entr}/bin/entr -d ${build}
        done
      '';

  test =
    checkedShellScript "${binPrefix}test"
      ''
        export PATH=${elmPackages.elm}/bin:"$PATH"
        cd "${settings.webappSrc}"
        ${elmPackages.elm-test}/bin/elm-test
      '';
}
