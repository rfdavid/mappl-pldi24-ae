{ lib, tree, fetchFromGitHub, callPackage, ocaml-ng, rustPlatform }:

let 
  # Dice uses OCaml 4.09. Since we only need the `dice` 
  # executable, we use our own OCaml version and libraries 
  # without interfering with Mappl's OCaml environment.
  ocamlPackages = ocaml-ng.ocamlPackages_4_09; 
in

let 
  cudd = callPackage ./mlcuddidl.nix { ocamlPackages = ocamlPackages; };  
  yojson = callPackage ./yojson.nix { ocamlPackages = ocamlPackages; };
in 

ocamlPackages.buildDunePackage rec {
  pname = "dice";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "SHoltzen";
    repo = "dice";
    rev = "0ea228edac87f3ef7b0785c23786a3696b912c55";
    sha256 = "sha256-mQsvsLCxq4nrA/J6yXjun/2wICy289saclOyQe6W/iQ=";
  };

  nativeBuildInputs = [
    ocamlPackages.menhir
  ];

  propagatedBuildInputs = [
    cudd
    ocamlPackages.menhir
    ocamlPackages.core
    ocamlPackages.ounit2
    ocamlPackages.ppx_sexp_conv
    ocamlPackages.sexplib
    ocamlPackages.core_bench
    ocamlPackages.ppx_deriving
    yojson
    ocamlPackages.ctypes
    ocamlPackages.bignum
    ocamlPackages.menhirLib
    ocamlPackages.ppx_jane
  ];
}
