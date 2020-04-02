{ pythonPackages
, checkedShellScript
, settings
, entr
}:

let
  binPrefix =
    "${settings.binPrefix}docs-";

  sphinx =
    pythonPackages.sphinx;
in
rec {
  build =
    checkedShellScript "${binPrefix}build"
      ''
        ${sphinx}/bin/sphinx-build "${settings.docsSrc}" "${settings.docsDir}"
      '';

  watch =
    checkedShellScript "${binPrefix}watch"
      ''
       find "${settings.docsSrc}" | ${entr}/bin/entr -r ${build}
      '';
}
