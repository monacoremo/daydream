{ mkDerivation, base, bytestring, containers, filepath, HUnit
, megaparsec, protolude, scientific, stdenv, test-framework
, test-framework-hunit, text
}:
mkDerivation {
  pname = "configurator-pg";
  version = "0.2.1";
  sha256 = "92b0c45cbdac2c1856d4493d2e1b2c006df1ddc3e17100b5419f3d044c809f42";
  libraryHaskellDepends = [
    base containers megaparsec protolude scientific text
  ];
  testHaskellDepends = [
    base bytestring filepath HUnit protolude test-framework
    test-framework-hunit text
  ];
  homepage = "https://github.com/robx/configurator-pg";
  description = "Reduced parser for configurator-ng config files";
  license = stdenv.lib.licenses.bsd3;
}
