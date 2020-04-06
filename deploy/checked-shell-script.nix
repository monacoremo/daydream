{ writeTextFile
, runtimeShell
, stdenv
, shellcheck
}:
let
  writeBin =
    name: text:
      writeTextFile {
        inherit name;
        executable = true;
        destination = "/bin/${name}";
        text =
          ''
            #!${runtimeShell}
            set -euo pipefail

            ${text}
          '';
        checkPhase =
          ''
            # check syntax
            ${stdenv.shell} -n $out/bin/${name}

            # check for shellcheck recommendations
            ${shellcheck}/bin/shellcheck $out/bin/${name}
          '';
      };
in
name: text:
stdenv.mkDerivation rec {
  inherit name;

  bin = writeBin name text;

  phases = [ "installPhase" ];

  installPhase =
    ''
      ln -s ${bin}/bin/${name} $out
    '';
}
