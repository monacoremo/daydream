{ settings
, writeText
, checkedShellScript
, entr
, haskellPackages
}:
let
  postgrest =
    haskellPackages.postgrest;

  postgrestConf =
    writeText "postgrest.conf"
      ''
        db-uri = "$(${settings.vars.dbApiserverURI})"
        db-schema = "api"
        db-anon-role = "anonymous"

        pre-request = "auth.authenticate"

        server-unix-socket = "$(${settings.vars.apiSocket})"
      '';

  binPrefix =
    "${settings.binPrefix}api-";
in
rec {
  run =
    checkedShellScript "${binPrefix}run"
      ''
        mkdir -p "${settings.apiDir}"
        exec ${postgrest}/bin/postgrest ${postgrestConf}
      '';

  watch =
    checkedShellScript "${binPrefix}watch"
      ''
        find "${settings.dbSrc}" | ${entr}/bin/entr -d -r ${run}
      '';
}
