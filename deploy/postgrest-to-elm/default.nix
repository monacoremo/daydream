{ haskellPackages }:

haskellPackages.callCabal2nix "postgrest-to-elm" ./. {}
