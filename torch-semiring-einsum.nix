{ python3Packages, fetchPypi }:

python3Packages.buildPythonPackage rec {
  pname = "torch_semiring_einsum";
  version = "1.1.0";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-dbAVY5MRnQFaZQ56XlU2x1ntBL1LG08SXgDzHL7aNr0=";
  };
  doCheck = false;
}
