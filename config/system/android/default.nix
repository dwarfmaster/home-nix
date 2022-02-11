{ config, pkgs, ... }:

{
  home.packages = [ pkgs.android-udev-rules ];
}
