{ stdenv, lib, fetchurl
, autoPatchelfHook
}:

stdenv.mkDerivation {
  name = "intel-unite";
  version = "??";
  src = fetchurl {
    url = "";
    sha256 = "";
  };

  nativeBuildInputs = [
    autoPatchelfHook
  ];

  buildInputs = [
    # TODO
  ];

  sourceRoot = ".";
  installPhase = ''
  '';

  meta = {
    homepage = "https://www.intel.fr/content/www/fr/fr/architecture-and-technology/unite/intel-unite-overview.html";
    description = "Open and Nearly Limitless Collaboration";
    platforms = lib.platforms.linux;
  };
}
