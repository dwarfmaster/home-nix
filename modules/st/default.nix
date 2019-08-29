{ pkgs, ... }:

let term = pkgs.st.override { conf = builtins.readFile ./st.h; };
in {
  packages = [ term ];
  misc.terminal = "${term}/bin/st";
}

