# List of package to install system-wide to all personal computers

{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wget
    gnufdisk
    vim
    lm_sensors
    htop
    git
    home-manager
  ];
}

