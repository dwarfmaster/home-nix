{ config, lib, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    layout = "fr";

    # Use startx as display-manager
    displayManager = {
      startx.enable = true;
      defaultSession = "none+xmonad";
    };
    desktopManager.xterm.enable = false;
    windowManager.xmonad.enable = true;

    # Support for touchpad
    libinput.enable = true;
    libinput.touchpad.tapping = false;
  };

  # Add dconf as dbus service
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
}
