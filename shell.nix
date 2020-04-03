let
  nixpkgsVersion =
    import deploy/nixpkgs-version.nix;

  pinnedPkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  packageOverrides =
    import deploy/overrides.nix;

  pkgs =
    import pinnedPkgs { config = { inherit packageOverrides; }; };

  project =
    pkgs.callPackage ./default.nix {};
in
pkgs.mkShell {
  name = "${project.settings.appName}-env";

  buildInputs = [
    project.api.run.bin
    project.api.watch.bin
    project.db.run.bin
    project.db.watch.bin
    project.db.setup.bin
    project.db.startDaemon.bin
    project.db.stopDaemon.bin
    project.db.test.bin
    project.deployLocal.mkEnv.bin
    project.deployLocal.run.bin
    project.deployLocal.watch.bin
    project.ingress.run.bin
    project.webapp.build.bin
    project.webapp.watch.bin
    project.webapp.generatePostgrestBindings.bin
    project.tests.run.bin
    project.tests.watch.bin
    project.docs.build.bin
    project.docs.watch.bin
    project.nixpkgsUpdate.bin
    project.geckodriver
    project.postgresql
    project.postgrest
    project.python
    pkgs.bash
    pkgs.firefox
    pkgs.curl
    pkgs.entr
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.silver-searcher
    pkgs.cabal2nix
    pkgs.dbeaver
  ];

  shellHook = ''
    tmpdir="$(mktemp -d)"
    source "$(${project.deployLocal.mkEnv} . "$tmpdir")"
    trap 'rm -rf $tmpdir' exit
    echo "Environment for ${project.settings.appName} set up in $tmpdir"

    # disable line wrap in psql
    export PAGER="less -S"
  '';
}
