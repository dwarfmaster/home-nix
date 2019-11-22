{ pkgs, ... }:

# TODO Not enough !
with pkgs.main; python35Packages.buildPythonPackage rec {
  name = "msi-perkeyrgb-${version}";
  version = "1.4";

  src = fetchFromGitHub {
    owner  = "Askannz";
    repo   = "msi-perkeyrgb";
    rev    = "v${version}";
    sha256 = "1yp705gaxacmwprzj5xp4bxy4w18488bn2gpi3kk5pdh4569nkhg";
  };

  buildInputs = with python35Packages; [ hidapi setuptools ];
}

