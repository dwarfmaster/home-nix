{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      # 3D
      solvespace # generic CAD
      freecad    # generic CAD

      # Misc
      leocad     # LEGO models designer
      ;
  };
}
