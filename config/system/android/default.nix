{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = [ pkgs.android-udev-rules ];
}
