{ checkedShellScript
, jq
, curl
}:
let
  refUrl =
    https://api.github.com/repos/nixos/nixpkgs/git/ref/heads/nixpkgs-unstable;

  githubV3Header =
    "Accept: application/vnd.github.v3+json";

  tarballUrlBase =
    https://github.com/nixos/nixpkgs/archive/;
in
checkedShellScript "nixpkgs-update"
  ''
    commitHash="$(${curl}/bin/curl "${refUrl}" -H "${githubV3Header}" | ${jq}/bin/jq -r .object.sha)"
    tarballUrl="${tarballUrlBase}$commitHash.tar.gz"
    tarballHash="$(nix-prefetch-url --unpack "$tarballUrl")"
    currentDate="$(date --iso)"

    cat << EOF
    {
      date = "$currentDate";
      rev = "$commitHash";
      tarballHash = "$tarballHash";
    }
    EOF
  ''
