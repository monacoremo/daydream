{ settings
, python
, checkedShellScript
, deployLocal
, curl
, entr
, firefox
, geckodriver
, silver-searcher
}:
let
  testPython =
    python.withPackages
      (
        ps: [
          ps.pytest
          ps.requests
          ps.selenium
        ]
      );

  binPrefix =
    "${settings.binPrefix}tests-";
in
rec {
  run =
    checkedShellScript "${binPrefix}run"
      ''
        export PATH=${geckodriver}/bin:${firefox}/bin:"$PATH"

        ${testPython}/bin/py.test "${settings.sourceDir}"/tests "$@"
      '';

  runWithTmpEnv =
    checkedShellScript "${binPrefix}withtmpenv-run"
      ''
        ${deployLocal.withTmpEnv} ${run} "$@"
      '';

  watch =
    checkedShellScript "${binPrefix}watch"
      ''
        while true; do
          ${silver-searcher}/bin/ag -l . "${settings.sourceDir}" | \
            ${entr}/bin/entr -d ${run}
        done
      '';
}
