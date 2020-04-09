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

        mkdir -p "${settings.testsDir}"
        cd "${settings.testsDir}"

        ${testPython}/bin/py.test "${settings.sourceDir}"/tests "$@"
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
