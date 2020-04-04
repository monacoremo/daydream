{ settings
, python
, checkedShellScript
, deployLocal
, curl
, entr
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
        ${testPython}/bin/py.test "${settings.sourceDir}"/tests "$@"
      '';

  watch =
    checkedShellScript "${binPrefix}watch"
      ''
        find "${settings.sourceDir}" | ${entr}/bin/entr ${run}
      '';
}
