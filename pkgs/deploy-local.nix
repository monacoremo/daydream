{ stdenv
, shellcheck
, python
, pwgen
, runtimeShell
, checkedShellScript
, settings
, api
, db
, ingress
, webapp
, docs
, logmux
, randomfreeport
, module
}:
let
  binPrefix =
    "${settings.binPrefix}local-";
in
module "deploy-local"
  rec {
    run =
      checkedShellScript "${binPrefix}run"
        ''
          set -e
          trap "kill 0" exit

          ${resetLogs}
          ${tailLogs} &

          ${webapp.build} 2&>> "${settings.webappLogfile}"
          ${docs.build} 2&>> "${settings.docsLogfile}"

          ${db.run} 2&>> "${settings.dbLogfile}" &
          ${api.run} 2&>> "${settings.apiLogfile}" &
          ${ingress.run} 2&>> "${settings.ingressLogfile}" &

          wait
        '';

    watch =
      checkedShellScript "${binPrefix}watch"
        ''
          set -e
          trap "kill 0" exit

          ${resetLogs}
          ${tailLogs} &

          ${db.watch} 2&>> "${settings.dbLogfile}" &
          ${api.watch} 2&>> "${settings.apiLogfile}" &
          ${ingress.run} 2&>> "${settings.ingressLogfile}" &
          ${webapp.watch} 2&>> "${settings.webappLogfile}" &
          ${docs.watch} 2&>> "${settings.docsLogfile}" &

          wait
        '';

    tailLogs =
      checkedShellScript "${binPrefix}taillogs"
        ''
          ${logmux}/bin/logmux \
              "${settings.dbLogfile}?label=db&color=red" \
              "${settings.apiLogfile}?label=api&color=blue" \
              "${settings.ingressLogfile}?label=ingress&color=green" \
              "${settings.ingressDir}/logs/access.log?label=ingress-access&color=bright_green" \
              "${settings.webappLogfile}?label=webapp&color=cyan" \
              "${settings.docsLogfile}?label=docs&color=blue"
        '';

    resetLogs =
      checkedShellScript "${binPrefix}resetlogs"
        ''
          mkdir -p "${settings.ingressDir}/logs"
          true > "${settings.ingressDir}/logs/access.log"
          true > "${settings.dbLogfile}"
          true > "${settings.apiLogfile}"
          true > "${settings.ingressLogfile}"
          true > "${settings.webappLogfile}"
          true > "${settings.docsLogfile}"
        '';

    mkEnv =
      checkedShellScript "${binPrefix}mkenv"
        ''
          sourcedir=$(realpath "$1")
          basedir=$(realpath "$2")
          port="$3"
          envfile="$basedir"/env

          mkdir -p "$basedir"

          cat << EOF > "$envfile"
          #!${runtimeShell}

          export ${settings.vars.sourceDir}="$sourcedir"
          export ${settings.vars.port}="$port"
          export ${settings.vars.dir}="$basedir"
          export ${settings.vars.URI}="http://localhost:\${settings.port}"
          export ${settings.vars.dbDir}="\${settings.dir}/db"
          export ${settings.vars.dbLogfile}="\${settings.dir}/db.log"
          export ${settings.vars.dbSrc}="\${settings.sourceDir}/db"
          export ${settings.vars.dbHost}="\${settings.dbDir}"
          export ${settings.vars.dbName}=postgres
          export ${settings.vars.dbSuperuser}=postgres
          export ${settings.vars.dbApiserver}=authenticator
          export ${settings.vars.dbURI}="postgresql:///\${settings.dbName}?host=\${settings.dbHost}"
          export ${settings.vars.dbSuperuserPassword}=$(${pwgen}/bin/pwgen 32 1)
          export ${settings.vars.dbApiserverPassword}=$(${pwgen}/bin/pwgen 32 1)
          export ${settings.vars.dbSetupHost}="\${settings.dbDir}/setupsocket"
          export ${settings.vars.dbSetupURI}="postgresql:///\${settings.dbName}?host=\${settings.dbSetupHost}&user=\${settings.dbSuperuser}&password=\${settings.dbSuperuserPassword}"
          export ${settings.vars.dbSuperuserURI}="\${settings.dbURI}&user=\${settings.dbSuperuser}&password=\${settings.dbSuperuserPassword}"
          export ${settings.vars.dbApiserverURI}="\${settings.dbURI}&user=\${settings.dbApiserver}&password=\${settings.dbApiserverPassword}"

          export ${settings.vars.apiLogfile}="\${settings.dir}/api.log"
          export ${settings.vars.apiDir}="\${settings.dir}/api"
          export ${settings.vars.apiSocket}="\${settings.apiDir}/postgrest.sock"
          export ${settings.vars.apiConfig}="\${settings.apiDir}/postgrest.conf"
          export ${settings.vars.apiURI}="http://unix:\${settings.apiSocket}:/"

          export ${settings.vars.webappLogfile}="\${settings.dir}/webapp.log"
          export ${settings.vars.webappSrc}="\${settings.sourceDir}/webapp"
          export ${settings.vars.webappDir}="\${settings.dir}/webapp"
          export ${settings.vars.webappWebroot}="\${settings.webappDir}/webroot"

          export ${settings.vars.ingressLogfile}="\${settings.dir}/ingress.log"
          export ${settings.vars.ingressDir}="\${settings.dir}/ingress"

          export ${settings.vars.docsLogfile}="\${settings.dir}/docs.log"
          export ${settings.vars.docsSrc}="\${settings.sourceDir}/docs"
          export ${settings.vars.docsDir}="\${settings.dir}/docs"

          export ${settings.vars.testsDir}="\${settings.dir}/tests"

          export TESTS_SERVICE_BIN="${run}"
          export TESTS_SERVICE_URI="\${settings.URI}"
          export TESTS_DBTEST_BIN="${db.test}"
          export TESTS_WEBAPP_BIN="${webapp.test}"

          EOF

          ${stdenv.shell} -n "$envfile"
          ${shellcheck}/bin/shellcheck "$envfile"

          echo "$envfile"
        '';
  }
