# Define the user 'luc'

{ config, lib, pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.luc = {
    isNormalUser = true;
    home = "/home/luc";
    extraGroups = [ "networkmanager" "wheel" "adbusers" ];
    shell = "/run/current-system/sw/bin/zsh";

    openssh.authorizedKeys = {
      keyFiles = [
        ./luc.pub
      ];
    };
  };
}

