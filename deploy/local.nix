{ stdenv, checkedShellScript, runtimeShell, shellcheck, pwgen, ncurses,
utillinux, writeText, envsubst, unixtools, python }:
{ settings, db, api, ingress, webapp }:


let
  logmux =
    ./logmux.py;
in
rec {
  run =
    checkedShellScript.writeBin "fullstack-local-run"
      ''
        set -e
        trap "kill 0" exit

        ${resetLogs}/bin/fullstack-local-resetlogs
        ${tailLogs}/bin/fullstack-local-taillogs 1>&2 &

        ${webapp.build}/bin/fullstack-webapp-build 2&>> "$FULLSTACK_WEBAPP_LOGFILE"

        ${db.run}/bin/fullstack-db-run 2&>> "$FULLSTACK_DB_LOGFILE" &
        ${api.run}/bin/fullstack-api-run 2&>> "$FULLSTACK_API_LOGFILE" &
        ${ingress.run}/bin/fullstack-ingress-run 2&>> "$FULLSTACK_INGRESS_LOGFILE" &

        wait
      '';

  watch =
    checkedShellScript.writeBin "fullstack-local-watch"
      ''
        set -e
        trap "kill 0" exit

        ${resetLogs}/bin/fullstack-local-resetlogs
        ${tailLogs}/bin/fullstack-local-taillogs 1>&2 &

        ${db.watch}/bin/fullstack-db-watch 2&>> "$FULLSTACK_DB_LOGFILE" &
        ${api.watch}/bin/fullstack-api-watch 2&>> "$FULLSTACK_API_LOGFILE" &
        ${ingress.run}/bin/fullstack-ingress-run 2&>> "$FULLSTACK_INGRESS_LOGFILE" &
        ${webapp.watch}/bin/fullstack-webapp-watch 2&>> "$FULLSTACK_WEBAPP_LOGFILE" &

        wait
      '';

  tailLogs =
    checkedShellScript.writeBin "fullstack-local-taillogs"
      ''
        ${python}/bin/python ${logmux} \
            "$FULLSTACK_DB_LOGFILE?label=db&color=red" \
            "$FULLSTACK_API_LOGFILE?label=api&color=blue" \
            "$FULLSTACK_INGRESS_LOGFILE?label=ingress&color=green" \
            "$FULLSTACK_INGRESS_DIR/logs/access.log?label=ingress-access&color=bright_green" \
            "$FULLSTACK_WEBAPP_LOGFILE?label=webapp&color=cyan"
      '';

  resetLogs =
    checkedShellScript.writeBin "fullstack-local-resetlogs"
      ''
        mkdir -p "$FULLSTACK_INGRESS_DIR/logs"
        true > "$FULLSTACK_INGRESS_DIR/logs/access.log"
        true > "$FULLSTACK_DB_LOGFILE"
        true > "$FULLSTACK_API_LOGFILE"
        true > "$FULLSTACK_INGRESS_LOGFILE"
        true > "$FULLSTACK_WEBAPP_LOGFILE"
      '';

  mkEnv =
    checkedShellScript.writeBin "fullstack-local-mkenv"
      ''
        sourcedir=$(realpath "$1")
        basedir=$(realpath "$2")
        envfile="$basedir"/env

        mkdir -p "$basedir"

        cat << EOF > "$envfile"
        #!${runtimeShell}

        export ${settings.vars.sourceDir}="$sourcedir"
        export ${settings.vars.port}=8000
        export ${settings.vars.dir}="$basedir"
        export ${settings.vars.URI}="http://localhost:\${settings.port}"
        export ${settings.vars.dbDir}="\${settings.dir}/db"
        export ${settings.vars.dbLogfile}="\${settings.dir}/db.log"
        export ${settings.vars.dbSrc}="\${settings.sourceDir}/db"
        export ${settings.vars.dbHost}="\${settings.dbDir}"
        export ${settings.vars.dbName}=postgres
        export ${settings.vars.dbSuperuser}=postgres
        export ${settings.vars.dbURI}="postgresql:///\${settings.dbName}?host=\${settings.dbHost}"
        export ${settings.vars.dbSuperuserPassword}=$(${pwgen}/bin/pwgen 32 1)
        export ${settings.vars.dbApiserverPassword}=$(${pwgen}/bin/pwgen 32 1)
        export ${settings.vars.dbSetupHost}="\${settings.dbDir}/setupsocket"
        export ${settings.vars.dbSetupURI}="postgresql:///\${settings.dbName}?host=\${settings.dbSetupHost}&user=\${settings.dbSuperuser}&password=\${settings.dbSuperuserPassword}"
        export ${settings.vars.dbSuperuserURI}="\${settings.dbURI}&user=\${settings.dbSuperuser}&password=\${settings.dbSuperuserPassword}"
        export ${settings.vars.dbApiserverURI}="\${settings.dbURI}&user=authenticator&password=\${settings.dbApiserverPassword}"

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

        # psql variables for convenience
        export PGHOST="\${settings.dbHost}"
        export PGDATABASE="\${settings.dbName}"
        export PGUSER="\${settings.dbSuperuser}"
        export PGPASSWORD="\${settings.dbSuperuserPassword}"

        EOF

        ${stdenv.shell} -n "$envfile"
        ${shellcheck}/bin/shellcheck "$envfile"

        echo "$envfile"
      '';
}
