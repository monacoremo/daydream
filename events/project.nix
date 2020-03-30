{ mkDerivation, base, hasql, servant, stdenv }:
mkDerivation {
  pname = "events";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hasql servant ];
  license = stdenv.lib.licenses.mit;
}
