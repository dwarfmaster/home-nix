{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wget
    gnufdisk
    vim
    lm_sensors
    htop
    git
    ntfs3g
    exfat-utils
  ];

  programs = {
    zsh.enable = true;
    adb.enable = true;
  };

  # System completion for ZSH
  environment.pathsToLink = [ "/share/zsh" ];
  # Docker support
  virtualisation.docker.enable = true;
}
