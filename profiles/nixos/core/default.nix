{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./env
    ./locale
    ./nix
    ./shells
  ];

  # Docker support
  virtualisation.docker.enable = true;

  security.protectKernelImage = true;
  services.earlyoom.enable = true;

  users.mutableUsers = false;

  # Enable PAM for screen locker
  security.pam.services.i3lock.enable = true;
  security.pam.services.i3lock-color.enable = true;
}
