{ mkDerivation, base, hasql, stdenv }:
mkDerivation {
  pname = "events";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hasql ];
  license = stdenv.lib.licenses.mit;
}
