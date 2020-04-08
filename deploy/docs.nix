{ python
, checkedShellScript
, settings
, entr
, silver-searcher
}:
let
  binPrefix =
    "${settings.binPrefix}docs-";

  sphinx =
    python.withPackages (ps: [ ps.sphinx ]);
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
        while true; do
          ${silver-searcher}/bin/ag -l . "${settings.docsSrc}" | \
            ${entr}/bin/entr -d ${build}
        done
      '';
}
