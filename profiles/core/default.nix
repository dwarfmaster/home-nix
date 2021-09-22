{ config, lib, pkgs, ... }:

{
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
}
