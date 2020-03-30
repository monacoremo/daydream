{ stdenv, checkedShellScript, runtimeShell, shellcheck, pwgen, ncurses,
utillinux, writeText, envsubst, unixtools, python }:
{ db, api, ingress, webapp }:


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

        export FULLSTACK_SRC="$sourcedir"
        export FULLSTACK_PORT=8000
        export FULLSTACK_DIR="$basedir"
        export FULLSTACK_URI="http://localhost:\$FULLSTACK_PORT"
        export FULLSTACK_DB_DIR="\$FULLSTACK_DIR/db"
        export FULLSTACK_DB_LOGFILE="\$FULLSTACK_DIR/db.log"
        export FULLSTACK_DB_SRC="$sourcedir/db"
        export FULLSTACK_DB_HOST="\$FULLSTACK_DB_DIR"
        export FULLSTACK_DB_DBNAME=postgres
        export FULLSTACK_DB_SUPERUSER=postgres
        export FULLSTACK_DB_URI="postgresql:///\$FULLSTACK_DB_DBNAME?host=\$FULLSTACK_DB_HOST"
        export FULLSTACK_DB_SUPERUSER_PW=$(${pwgen}/bin/pwgen 32 1)
        export FULLSTACK_DB_APISERVER_PW=$(${pwgen}/bin/pwgen 32 1)
        export FULLSTACK_DB_SETUPHOST="\$FULLSTACK_DB_DIR/setupsocket"
        export FULLSTACK_DB_SUPERUSER_SETUP_URI="postgresql:///\$FULLSTACK_DB_DBNAME?host=\$FULLSTACK_DB_SETUPHOST&user=\$FULLSTACK_DB_SUPERUSER&password=\$FULLSTACK_DB_SUPERUSER_PW"
        export FULLSTACK_DB_SUPERUSER_URI="\$FULLSTACK_DB_URI&user=\$FULLSTACK_DB_SUPERUSER&password=\$FULLSTACK_DB_SUPERUSER_PW"
        export FULLSTACK_DB_APISERVER_URI="\$FULLSTACK_DB_URI&user=authenticator&password=\$FULLSTACK_DB_APISERVER_PW"

        export FULLSTACK_API_LOGFILE="\$FULLSTACK_DIR/api.log"
        export FULLSTACK_API_DIR="\$FULLSTACK_DIR/api"
        export FULLSTACK_API_SOCKET="\$FULLSTACK_API_DIR/postgrest.sock"
        export FULLSTACK_API_CONFIG="\$FULLSTACK_API_DIR/postgrest.conf"
        export FULLSTACK_API_URI="http://unix:\$FULLSTACK_API_SOCKET:/"

        export FULLSTACK_WEBAPP_LOGFILE="\$FULLSTACK_DIR/webapp.log"
        export FULLSTACK_WEBAPP_SRC="$sourcedir/webapp"
        export FULLSTACK_WEBAPP_DIR="\$FULLSTACK_DIR/webapp"
        export FULLSTACK_WEBAPP_WEBROOT="\$FULLSTACK_WEBAPP_DIR/webroot"

        export FULLSTACK_INGRESS_LOGFILE="\$FULLSTACK_DIR/ingress.log"
        export FULLSTACK_INGRESS_DIR="\$FULLSTACK_DIR/ingress"

        # psql variables for convenience
        export PGHOST="\$FULLSTACK_DB_HOST"
        export PGDATABASE="\$FULLSTACK_DB_DBNAME"
        export PGUSER="\$FULLSTACK_DB_SUPERUSER"
        export PGPASSWORD="\$FULLSTACK_DB_SUPERUSER_PW"

        EOF

        ${stdenv.shell} -n "$envfile"
        ${shellcheck}/bin/shellcheck "$envfile"

        echo "$envfile"
      '';
}
