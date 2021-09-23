{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      gcc       # C/C++ compiler
      gdb       # C/C++ debugger
      ddd       # Graphical frontendfor GDB
      pkgconfig # Library finder
      ;
  };
}
