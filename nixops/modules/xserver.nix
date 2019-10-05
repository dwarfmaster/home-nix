# Configure a simple xserver with no display manager
# User must invoke startx after loging to tty

{ config, lib, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    layout = "fr";
    displayManager.startx.enable = true;
    desktopManager.default = "none";
    desktopManager.xterm.enable = false;

    libinput.enable = true;
    libinput.tapping = false;
  };
}

