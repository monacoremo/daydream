{ mkDerivation
, aeson
, base
, bound
, elm-syntax
, fetchgit
, haskell-to-elm
, hpack
, http-types
, servant
, servant-multipart
, stdenv
, text
}:
mkDerivation {
  pname = "servant-to-elm";
  version = "0.4.1.0";
  src = fetchgit {
    url = "https://github.com/folq/servant-to-elm.git";
    sha256 = "1x3vz60scj4pg0xhvc7kr00gp0b42bn030h2ldxc3yjpz4289fjj";
    rev = "f4e9b14940e95868dbe24ebc263b2d60e1cabc65";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    bound
    elm-syntax
    haskell-to-elm
    http-types
    servant
    servant-multipart
    text
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson
    base
    bound
    elm-syntax
    haskell-to-elm
    http-types
    servant
    servant-multipart
    text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/folq/servant-to-elm#readme";
  description = "Automatically generate Elm clients for Servant APIs";
  license = stdenv.lib.licenses.bsd3;
}
