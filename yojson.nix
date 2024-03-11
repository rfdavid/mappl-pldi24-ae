{ fetchFromGitHub, fetchurl, ocamlPackages }:

ocamlPackages.buildDunePackage rec {
  pname = "yojson";
  version = "1.5.0";
  src = fetchurl {
    url = "https://github.com/ocaml-community/yojson/releases/download/${version}/yojson-${version}.tbz";
    hash = "sha256-cKD5cv7evXNlRuroMuJVdT0yFKizKDWXrhEld9cMizM=";
  };

  nativeBuildInputs = [ ocamlPackages.cppo ];
  propagatedBuildInputs = [
    ocamlPackages.cppo
    ocamlPackages.easy-format
    ocamlPackages.biniou
  ];
}
