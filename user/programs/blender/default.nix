{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      blender
      ;
  };
}