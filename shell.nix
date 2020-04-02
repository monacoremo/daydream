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
    fullstack.api.run.bin
    fullstack.api.watch.bin
    fullstack.db.run.bin
    fullstack.db.watch.bin
    fullstack.db.setup.bin
    fullstack.db.startDaemon.bin
    fullstack.db.stopDaemon.bin
    fullstack.deployLocal.mkEnv.bin
    fullstack.deployLocal.run.bin
    fullstack.deployLocal.watch.bin
    fullstack.ingress.run.bin
    fullstack.webapp.build.bin
    fullstack.webapp.watch.bin
    fullstack.webapp.generatePostgrestBindings.bin
    fullstack.tests.run.bin
    fullstack.tests.watch.bin
    fullstack.geckodriver
    fullstack.postgresql
    fullstack.postgrest
    fullstack.python
    pkgs.bash
    pkgs.firefox
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
