{ pkgs }:

{ postgrestToElm = pkgs.haskellPackages.callCabal2nix "postgrest-to-elm" ./. {};
}
