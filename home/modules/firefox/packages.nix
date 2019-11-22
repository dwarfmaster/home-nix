general:

let
  pkgs = general.pkgs.main;
in with pkgs; {
  rofi-tab-switcher = stdenv.mkDerivation rec {
    name = "rofi-tab-switcher-${version}";
    version = "403f3a243ff90181a356c92c3855de16d8256b8a";

    meta = {
      description = "Necessary for the rofi-tab-switcher firefox extension";
      homepage    = "https://github.com/blackhole89/rofi-tab-switcher";
      maintainers = with stdenv.lib.maintainers; [ lucas8 ];
      license     = stdenv.lib.licenses.mpl20;
      platforms   = with stdenv.lib.platforms; linux;
    };

    src = fetchFromGitHub {
      owner  = "blackhole89";
      repo   = "rofi-tab-switcher";
      rev    = "403f3a243ff90181a356c92c3855de16d8256b8a";
      sha256 = "11fyjlgvydmwsckgriw3cif9rq88h74p9997nmgxjwl4npxc51ij";
    };

    patchPhase = ''
      sed -i -e 's:/usr/bin/python:${pkgs.python3}/bin/python:' rofiface.py
    '';

    installPhase = ''
      mkdir -p $out/bin
      install -m 700 rofiface.py $out/bin
    '';
  };
}

