{ config, lib, pkgs, ... }:

{
  # By default, no window manager nor DE is installed, and the display manager
  # only allows to log in  to xterm. The display manager installed is lightdm.
  # By setting startx as the display manager, I can run the graphic session
  # with startx.
  services.xserver = {
    enable = true;
    autorun = false;
    displayManager.startx.enable = true;

    enableTCP = false;
    enableCtrlAltBackspace = true;

    layout = "fr";

    # Support for touchpad
    libinput.enable = true;
    libinput.touchpad.tapping = false;
  };
}
