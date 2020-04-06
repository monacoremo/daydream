{ stdenv, buildPythonPackage, fetchPypi, click, pytest }:

buildPythonPackage rec {
  version = "0.1";
  pname = "logmux";

  src = fetchPypi {
    inherit pname version;
    sha256 = "13f9f196f330c7c2c5d7a5cf91af894110ca0215ac051b5844701f2bfd934d52";
  };

  checkInputs = [ pytest ];
  propagatedBuildInputs = [ click ];

  checkPhase = ''
    py.test tests.py
  '';

  meta = with stdenv.lib; {
    homepage = https://github.com/monacoremo/logmux;
    description = "Logmux tails multiple log files, labels their lines and outputs everything in one stream";
    license = licenses.mit;
  };
}
