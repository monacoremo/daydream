{ mkDerivation, aeson, base, bound, elm-syntax, fetchgit
, generics-sop, hpack, stdenv, text, time, unordered-containers
}:
mkDerivation {
  pname = "haskell-to-elm";
  version = "0.3.0.0";
  src = fetchgit {
    url = "https://github.com/folq/haskell-to-elm.git";
    sha256 = "0lqwyckb7shz416msb6cnzjmmc77wxqcwwsi80v7q2ihygzbj68a";
    rev = "335428ce592f7ad680faf12572f573348b45ec15";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bound elm-syntax generics-sop text time
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bound elm-syntax generics-sop text time
    unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/folq/haskell-to-elm#readme";
  description = "Generate Elm types and JSON encoders and decoders from Haskell types";
  license = stdenv.lib.licenses.bsd3;
}
