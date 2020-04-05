{ pkgs }:

{
  events = pkgs.haskellPackages.callCabal2nix "events" ./. {};
}
