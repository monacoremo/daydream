{ stdenv, buildPythonPackage, fetchPypi, click, pytest }:

buildPythonPackage rec {
  version = "0.1.0";
  pname = "logmux";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0gqw3zbkl0qfd515pvlwbc1k4r4wwfc0jrr799yvx1hplybnfgih";
  };

  checkInputs = [ pytest ];
  propagatedBuildInputs = [ click ];

  checkPhase = ''
    py.test tests.py
  '';

  doCheck = false;

  meta = with stdenv.lib; {
    homepage = https://github.com/monacoremo/logmux;
    description = "Logmux tails multiple log files, labels their lines and outputs everything in one stream";
    license = licenses.mit;
  };
}
