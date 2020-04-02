{ settings
, writeText
, checkedShellScript
, postgresql
, entr
, md2sql
}:

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

  env =
    ''
      export PGDATA="${settings.dbDir}"
      export PGHOST="${settings.dbHost}"
      export PGUSER="${settings.dbSuperuser}"
      export PGDATABASE="${settings.dbName}"
    '';

  binPrefix =
    "${settings.binPrefix}db-";
in
rec {
  setup =
    checkedShellScript "${binPrefix}setup"
      ''
        ${env}

        cleanup() {
          ${postgresql}/bin/pg_ctl stop -m i
          kill 0
        }

        trap cleanup exit

        rm -rf "${settings.dbDir}"
        mkdir -p "${settings.dbDir}"

        # Initialize the PostgreSQL cluster
        pwfile=$(mktemp)
        echo "${settings.dbSuperuserPassword}" > "$pwfile"

        TZ=UTC ${postgresql}/bin/initdb --no-locale --encoding=UTF8 --nosync \
          -U "$PGUSER" -A password --pwfile="$pwfile"

        rm "$pwfile"

        mkdir -p "${settings.dbSetupHost}"

        ${postgresql}/bin/pg_ctl start \
          -o "-F -c listen_addresses=\"\" -k ${settings.dbSetupHost}"

        for f in "${settings.dbSrc}"/*.sql.md; do
          filename=$(basename -- "$f")
          targetpath="${settings.dbDir}/$filename.sql"
          sed -f ${md2sql} <"$f" >"$targetpath"
          ${postgresql}/bin/psql "${settings.dbSetupURI}" -f "$targetpath"
        done

        ${postgresql}/bin/psql "${settings.dbSetupURI}" << EOF
          alter role authenticator with password '${settings.dbApiserverPassword}';
        EOF

        ${postgresql}/bin/pg_ctl stop

        trap - exit

        rm -rf "${settings.dbSetupHost}"

        cat ${postgresConf} >> "${settings.dbDir}"/postgresql.conf
      '';

  run =
    checkedShellScript "${binPrefix}run"
      ''
        ${env}
        ${setup}

        exec ${postgresql}/bin/postgres -F -c listen_addresses="" \
          -k "${settings.dbHost}"
      '';

  startDaemon =
    checkedShellScript "${binPrefix}daemon-start"
      ''
        ${env}

        ${postgresql}/bin/pg_ctl start \
          -o "-F -c listen_addresses=\"\" -k ${settings.dbHost}"
      '';

  stopDaemon =
    checkedShellScript "${binPrefix}daemon-stop"
      ''
        ${env}

        ${postgresql}/bin/pg_ctl stop
      '';

  watch =
    checkedShellScript "${binPrefix}watch"
      ''
        find "${settings.dbSrc}" | ${entr}/bin/entr -d -r ${run}
      '';
}
