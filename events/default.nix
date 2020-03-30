let
  pkgs =
    import ./nixpkgs.nix { config = { packageOverrides = packageOverrides; }; };

  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
      };
    };
  };
in
{ events = pkgs.haskellPackages.callCabal2nix "events" ./. {};
}
