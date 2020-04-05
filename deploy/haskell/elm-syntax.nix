{ mkDerivation
, base
, bound
, deriving-compat
, fetchgit
, hashable
, hpack
, prettyprinter
, stdenv
, text
, unordered-containers
}:
mkDerivation {
  pname = "elm-syntax";
  version = "0.3.0.0";
  src = fetchgit {
    url = "https://github.com/folq/elm-syntax.git";
    sha256 = "17x7fwsim7ni7696r01njvxd929v3xgmcaj6m5b8427gk817pn79";
    rev = "0bed7db2095fa7c3c9367e94effe93e4bf62f54e";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base
    bound
    deriving-compat
    hashable
    prettyprinter
    text
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base
    bound
    deriving-compat
    hashable
    prettyprinter
    text
    unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/folq/elm-syntax#readme";
  description = "Elm syntax and pretty-printing";
  license = stdenv.lib.licenses.bsd3;
}
