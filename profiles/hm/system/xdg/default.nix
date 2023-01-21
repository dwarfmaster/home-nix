{
  config,
  lib,
  pkgs,
  ...
}:
# I will setup one file per XDG specification, and try to implement them. I
# consider some specifications don't apply to me, so I won't implement those.
# The specification list can be found there :
# https://specifications.freedesktop.org/
{
  imports = [
    ./autostart.nix
    ./basedir.nix
    ./desktop-entry.nix
    ./mime-apps.nix
  ];

  home.packages = [
    pkgs.gtk4.dev # Some utilities, most importantly gtk4-icon-browser
  ];

  services.korrvigs = {
    constants = {
      xdg-config = "${config.xdg.configHome}";
      xdg-cache = "${config.xdg.cacheHome}";
      xdg-data = "${config.xdg.dataHome}";
      xdg-state = "${config.xdg.stateHome}";
    };
    extraModules.xdg = ./xdg.pl;
  };
}
