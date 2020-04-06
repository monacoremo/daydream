{ haskellPackages }:

haskellPackages.callCabal2nix "events" ./. {}
