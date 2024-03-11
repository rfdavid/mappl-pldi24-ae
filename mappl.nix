{ buildDunePackage, ocamlPackages }:

buildDunePackage rec {
  pname = "mappl";
  version = "1.0";

  src = ./.;

  nativeBuildInputs = [ ocamlPackages.menhir ];
  propagatedBuildInputs = with ocamlPackages; [
    ocaml
    dune_3
    dune-build-info 
    utop
    ocp-indent
    core
    menhir
    menhirLib
    ppx_deriving
    ppx_jane
    core_unix
    findlib
    odoc
  ];
}
