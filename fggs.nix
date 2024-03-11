{ callPackage, python3Packages, python311Packages, cudaPackages }:

let
  torch-semiring-einsum = callPackage ./torch-semiring-einsum.nix { inherit python3Packages; };
  pynvml = python311Packages.pynvml.overrideAttrs (old: {
    propagatedBuildInputs = builtins.filter (input: input != cudaPackages.cudatoolkit) old.buildInputs;
  });
in

python3Packages.buildPythonPackage {
  pname = "fggs";
  version = "09b367";
  src = builtins.fetchGit {
    url = "https://github.com/diprism/fggs/";
    rev = "09b367365b6210a3865a15206054d964ffb32a7e";
  };
  postInstall = ''
    mkdir -p $out/bin
    cp -a bin/. $out/bin/
  '';
  propagatedBuildInputs = with python3Packages; [
    setuptools
    torch
    pydot
    tqdm
    torch-semiring-einsum
    pynvml
  ];
  format = "pyproject";
}
