{ checkedShellScript
, pgformatter
, gnused
, md2sql
, sql2md
, python3
, nodePackages
, elmPackages
, nixpkgs-fmt
, settings
, ormolu
}:
let
  autoformatMdSql =
    checkedShellScript "autoformat-md-sql"
      ''
        echo "Formatting $1"
        tmpfile="$(mktemp)"
        ${gnused}/bin/sed -f ${md2sql} < "$1" \
          | ${pgformatter}/bin/pg_format -u 1 -w 80 \
          | ${gnused}/bin/sed -f ${sql2md} > "$tmpfile"

        mv "$tmpfile" "$1"
      '';

  black =
    python3.withPackages (ps: [ ps.black ]);

  prettier =
    nodePackages.prettier;
in
checkedShellScript "autoformat"
  ''
    exec 1>/dev/null 2>/dev/null

    echo "Formatting Python code..."
    ${black}/bin/black "${settings.sourceDir}"/tests

    echo "Formatting Elm code..."
    ${elmPackages.elm-format}/bin/elm-format --yes \
      "${settings.sourceDir}"/webapp/src

    echo "Formatting Haskell code..."
    find "${settings.sourceDir}" -iname "*.hs" \
      -exec ${ormolu}/bin/ormolu --mode inplace {} \;

    echo "Formatting Nix code..."
    ${nixpkgs-fmt}/bin/nixpkgs-fmt "${settings.sourceDir}"

    echo "Formatting Markdown files..."
    ${prettier}/bin/prettier --write "${settings.sourceDir}/**/*.md"

    echo "Formatting SQL code embedded in Markdown files..."
    find "${settings.sourceDir}" -iname "*.sql.md" -exec ${autoformatMdSql} {} \;
  ''
