let
  pkgs =
    let
      pinnedPkgs =
        builtins.fetchTarball {
          url = "https://github.com/nixos/nixpkgs/archive/f9c81b5c148572c2a78a8c1d2c8d5d40e642b31a.tar.gz";
          sha256 = "0ff7zhqk7mjgsvqyp4pa9xjvv9cvp3mh0ss9j9mclgzfh9wbwzmf";
        };
    in
      import pinnedPkgs {};

  fullstack =
    pkgs.callPackage ./default.nix {};
in
pkgs.stdenv.mkDerivation {
  name = "fullstack";

  buildInputs = [
    fullstack.api.run
    fullstack.api.watch
    fullstack.db.run
    fullstack.db.watch
    fullstack.deployLocal.mkEnv
    fullstack.deployLocal.run
    fullstack.deployLocal.watch
    fullstack.ingress.run
    fullstack.postgresql
    fullstack.postgrest
    fullstack.python
    fullstack.webapp.build
    fullstack.webapp.watch
    fullstack.tests.run
    fullstack.tests.watch
    pkgs.bash
    pkgs.curl
    pkgs.entr
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.silver-searcher
  ];

  shellHook = ''
    tmpdir="$(mktemp -d)"
    source "$(fullstack-local-mkenv . "$tmpdir")"
    trap "rm -rf $tmpdir" exit
    echo "Environment set up in $tmpdir"
  '';
}
