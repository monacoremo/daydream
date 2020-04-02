{ writeText, checkedShellScript, postgresql, entr }:

let
  postgresConf =
    writeText "postgresql.conf"
      ''
        log_min_messages = warning
        log_min_error_statement = error

        log_min_duration_statement = 100  # ms

        log_connections = on
        log_disconnections = on
        log_duration = on
        #log_line_prefix = '[] '
        #log_statement = 'none'
        log_timezone = 'UTC'
      '';

  md2sql =
    ./md2sql.sed;
in
rec {
  setup =
    checkedShellScript.writeBin "fullstack-db-setup"
      ''
        set -euo pipefail

        export PGDATA="$FULLSTACK_DB_DIR"
        export PGHOST="$FULLSTACK_DB_HOST"
        export PGUSER="$FULLSTACK_DB_SUPERUSER"
        export PGDATABASE="$FULLSTACK_DB_DBNAME"

        cleanup() {
          ${postgresql}/bin/pg_ctl stop -m i
          kill 0
        }

        trap cleanup exit

        rm -rf "$FULLSTACK_DB_DIR"
        mkdir -p "$FULLSTACK_DB_DIR"

        # Initialize the PostgreSQL cluster
        pwfile=$(mktemp)
        echo "$FULLSTACK_DB_SUPERUSER_PW" > "$pwfile"

        TZ=UTC ${postgresql}/bin/initdb --no-locale --encoding=UTF8 --nosync \
          -U "$PGUSER" -A password --pwfile="$pwfile"

        rm "$pwfile"

        mkdir -p "$FULLSTACK_DB_SETUPHOST"

        ${postgresql}/bin/pg_ctl start \
          -o "-F -c listen_addresses=\"\" -k $FULLSTACK_DB_SETUPHOST"

        for f in "$FULLSTACK_DB_SRC"/*.sql.md; do
          filename=$(basename -- "$f")
          targetpath="$FULLSTACK_DB_DIR/$filename.sql"
          sed -f ${md2sql} <"$f" >"$targetpath"
          ${postgresql}/bin/psql "$FULLSTACK_DB_SUPERUSER_SETUP_URI" -f "$targetpath"
        done

        ${postgresql}/bin/psql "$FULLSTACK_DB_SUPERUSER_SETUP_URI" << EOF
          alter role authenticator with password '$FULLSTACK_DB_APISERVER_PW';
        EOF

        ${postgresql}/bin/pg_ctl stop

        trap - exit

        rm -rf "$FULLSTACK_DB_SETUPHOST"

        cat ${postgresConf} >> "$FULLSTACK_DB_DIR"/postgresql.conf
      '';

  run =
    checkedShellScript.writeBin "fullstack-db-run"
      ''
        set -euo pipefail

        export PGDATA="$FULLSTACK_DB_DIR"
        export PGHOST="$FULLSTACK_DB_HOST"
        export PGUSER="$FULLSTACK_DB_SUPERUSER"
        export PGDATABASE="$FULLSTACK_DB_DBNAME"

        ${setup}/bin/fullstack-db-setup

        exec ${postgresql}/bin/postgres -F -c listen_addresses="" \
          -k "$FULLSTACK_DB_HOST"
      '';

  startDaemon =
    checkedShellScript.writeBin "fullstack-db-daemon-start"
      ''
        set -euo pipefail

        export PGDATA="$FULLSTACK_DB_DIR"
        export PGHOST="$FULLSTACK_DB_HOST"
        export PGUSER="$FULLSTACK_DB_SUPERUSER"
        export PGDATABASE="$FULLSTACK_DB_DBNAME"

        ${postgresql}/bin/pg_ctl start \
          -o "-F -c listen_addresses=\"\" -k $FULLSTACK_DB_HOST"
      '';

  stopDaemon =
    checkedShellScript.writeBin "fullstack-db-daemon-stop"
      ''
        set -euo pipefail

        export PGDATA="$FULLSTACK_DB_DIR"
        export PGHOST="$FULLSTACK_DB_HOST"
        export PGUSER="$FULLSTACK_DB_SUPERUSER"
        export PGDATABASE="$FULLSTACK_DB_DBNAME"

        ${postgresql}/bin/pg_ctl stop
      '';

  watch =
    checkedShellScript.writeBin "fullstack-db-watch"
      ''
        find "$FULLSTACK_DB_SRC" | \
          ${entr}/bin/entr -d -r ${run}/bin/fullstack-db-run
      '';
}
