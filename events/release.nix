let
  config =
    {
      date = "2020-03-15";
      rev = "13e7a3e11272159b9b1fc41ec67f53c1088412ff";
      tarballHash = "1d6byw99i4wjwdz501r6b12i8nifwl86gjd43cjvg8f2d78fazpg";
    };

  pinnedPkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${config.rev}.tar.gz";
      sha256 = config.tarballHash;
    };

  pkgs =
    import pinnedPkgs { config = { packageOverrides = packageOverrides; }; };

  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
      };
    };
  };
in
{ events = pkgs.haskellPackages.callPackage ./project.nix {};
}
