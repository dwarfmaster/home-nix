{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [pkgs.stable.khal];
  xdg.configFile."khal/config".source = ./khal.conf;
}
