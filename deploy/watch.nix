{ entr, checkedShellScript, silver-searcher }:

checkedShellScript "watch"
  ''
    while true; do
      ${silver-searcher}/bin/ag -l . "$1" | ${entr}/bin/entr -rd "$2"
    done
  ''
