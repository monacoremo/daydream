{ settings
, python
, checkedShellScript
, deployLocal
, curl
, entr
, firefox
, geckodriver
, silver-searcher
, module
}:
let
  testPython =
    python.withPackages
      (
        ps: [
          ps.pytest
          #ps.pytest_xdist
          ps.requests
          ps.selenium
        ]
      );

  binPrefix =
    "${settings.binPrefix}tests-";
in
module "tests"
  rec {
    run =
      checkedShellScript "${binPrefix}run"
        ''
          export PATH=${geckodriver}/bin:${firefox}/bin:"$PATH"

          ${testPython}/bin/pytest "${settings.sourceDir}"/tests "$@"
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
