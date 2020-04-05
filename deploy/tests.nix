{ settings
, python
, checkedShellScript
, deployLocal
, curl
, entr
, firefox
, geckodriver
}:

let
  testPython =
    python.withPackages
      (ps: [
          ps.pytest
          ps.requests
          ps.selenium
      ]);

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
        find "${settings.sourceDir}" | ${entr}/bin/entr ${run}
      '';
}
