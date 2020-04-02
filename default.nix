{ pkgs }:

rec {
  postgresql =
    pkgs.postgresql_12.withPackages
      (
        ps: [
          ps.pgtap
        ]
      );

  python =
    pkgs.python38.withPackages
      (
        ps: [
          ps.pytest
          ps.requests
          ps.selenium
          ps.click
        ]
      );

  geckodriver =
    pkgs.geckodriver;

  appName = "fullstack";

  settings =
    pkgs.callPackage deploy/settings.nix { inherit appName; };

  events =
    (import events/default.nix).events;

  postgrestToElm =
    (import postgrest-to-elm/default.nix).postgrestToElm;

  postgrest =
    pkgs.callPackage deploy/postgrest.nix {};

  webapp =
    pkgs.callPackage deploy/webapp.nix
      { inherit settings events db deployLocal checkedShellScript postgrestToElm; };

  ingress =
    pkgs.callPackage deploy/ingress.nix
      { inherit settings checkedShellScript; };

  api =
    pkgs.callPackage deploy/api.nix
      { inherit settings checkedShellScript postgrest; };

  db =
    pkgs.callPackage deploy/db.nix
      { inherit settings checkedShellScript postgresql; };

  checkedShellScript =
    pkgs.callPackage deploy/checked-shell-script.nix {};

  deployLocal =
    pkgs.callPackage deploy/local.nix
      { inherit settings checkedShellScript python db api ingress webapp; };

  tests =
    pkgs.callPackage deploy/tests.nix
      { inherit settings checkedShellScript python deployLocal; };
}
