{
  config,
  pkgs,
  lib,
  ...
}: let
  kodi = pkgs.kodi-wayland;
in {
  # Run kodi in wayland with cage kiosk
  # Supposedly faster than with xserver
  services.cage = {
    enable = true;
    user = "kodi";
    program = "${kodi}/bin/kodi-standalone";
  };

  users.users.kodi = {
    isNormalUser = true;
  };
}
