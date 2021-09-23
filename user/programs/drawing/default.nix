{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      gimp      # scalar image editor
      inkscape  # vectorial image editor
      gv        # PS utilities
      ;
  };
}
