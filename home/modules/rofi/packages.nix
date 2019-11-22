general:

let
  pkgs = general.pkgs.main;
in with pkgs; {
  rofi-calc = stdenv.mkDerivation rec {
    name = "rofi-calc-${version}";
    version = "7b1084cb3983f0a069a1113d29bc5af894f948ce";

    meta = {
      description = "Transform rofi in a calculator using qcalc";
      homepage    = "https://github.com/svenstaro/rofi-calc";
      maintainers = with stdenv.lib.maintainers; [ lucas8 ];
      license     = stdenv.lib.licenses.mit;
      platforms   = with stdenv.lib.platforms; linux;
    };

    src = fetchFromGitHub {
      owner  = "svenstaro";
      repo   = "rofi-calc";
      rev    = "7b1084cb3983f0a069a1113d29bc5af894f948ce";
      sha256 = "0c412pyd3nby6ghj4sfjpbdxz13189lizxib4p0h6wps2rykcl3v";
    };

    buildInputs = [ rofi libqalculate glib cairo pkg-config autoconf automake gnum4 libtool ];

    preConfigure = ''
      mkdir m4
      autoreconf -i
    '';

    postConfigure = ''
      sed -i                                                                                       \
          -e "s:^rofi_PLUGIN_INSTALL_DIR.*:rofi_PLUGIN_INSTALL_DIR=$out/share/rofi/plugins/:" \
          Makefile
    '';

    postInstall = ''
      libtool --finish $out/share/rofi/plugins
    '';
  };
}

