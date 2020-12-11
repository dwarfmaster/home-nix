general:

let

  pkgs = general.pkgs.main;

  term = pkgs.st.override { conf = builtins.readFile ./config.h; };

in {
  packages = [ term ];
  misc.terminal = "${term}/bin/st";
}

