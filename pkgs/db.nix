{ settings
, writeText
, checkedShellScript
, postgresql
, entr
, md2sql
, gnused
, postgresql_12
, perl
, module
}:
let
  postgresqlWithPerl =
    postgresql_12.overrideAttrs (
      attrs: {
        configureFlags =
          attrs.configureFlags ++ [ "--with-perl" ];

        buildInputs =
          attrs.buildInputs ++ [
            (perl.withPackages (ps: [ ps.EmailValid ]))
          ];
      }
    );

  postgresql =
    postgresqlWithPerl.withPackages
      (
        ps: [
          ps.pgtap
        ]
      );

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
module "db"
  rec {
    setup =
      checkedShellScript "${binPrefix}setup"
        ''
          ${env}

          echo "setting up in $PGDATA"

          cleanup() {
            ${postgresql}/bin/pg_ctl stop -m i
            kill 0
          }

          trap cleanup exit

          rm -rf "$PGDATA"
          mkdir -p "$PGDATA"

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
            ${gnused}/bin/sed -f ${md2sql} <"$f" >"$targetpath"
            ${postgresql}/bin/psql "${settings.dbSetupURI}" -v ON_ERROR_STOP=1 -f "$targetpath"
          done

          ${postgresql}/bin/psql "${settings.dbSetupURI}" << EOF
            alter role ${settings.dbApiserver}
              with password '${settings.dbApiserverPassword}';
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

    test =
      checkedShellScript "${binPrefix}test"
        ''
          ${setup} > /dev/null
          ${startDaemon} > /dev/null

          ${postgresql}/bin/psql "${settings.dbSuperuserURI}" -X -q << EOF
            begin;

            select tests.run() "test results";

            rollback;
          EOF

          ${stopDaemon} > /dev/null
        '';
  }
