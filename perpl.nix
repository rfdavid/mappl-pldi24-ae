{ stdenv, fetchFromGitHub, ghc }:

stdenv.mkDerivation rec {
  pname = "perpl";
  version = "73f88ee";

  src = fetchFromGitHub {
    owner = "diprism";
    repo = pname;
    rev = "73f88eef5d18ff63fda7d9be9f118db33868437e";
    sha256 = "sha256-ACQQMfhYjexky/a62NC+U3eDU6R8vde+Rt60DWG5nQA=";
  };

  buildInputs = [ ghc ];

  buildPhase = ''
    make -j$(nproc)
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./perplc $out/bin
  '';
}
