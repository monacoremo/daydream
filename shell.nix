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
pkgs.stdenv.mkDerivation {
  name = "${project.settings.appName}-env";

  buildInputs = [
    project.api.run.bin
    project.api.watch.bin
    project.db.run.bin
    project.db.watch.bin
    project.db.setup.bin
    project.db.startDaemon.bin
    project.db.stopDaemon.bin
    project.deployLocal.mkEnv.bin
    project.deployLocal.run.bin
    project.deployLocal.watch.bin
    project.ingress.run.bin
    project.webapp.build.bin
    project.webapp.watch.bin
    project.webapp.generatePostgrestBindings.bin
    project.tests.run.bin
    project.tests.watch.bin
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
  ];

  shellHook = ''
    tmpdir="$(mktemp -d)"
    source "$(${project.deployLocal.mkEnv} . "$tmpdir")"
    trap 'rm -rf $tmpdir' exit
    echo "Environment for ${project.settings.appName} set up in $tmpdir"
  '';
}
