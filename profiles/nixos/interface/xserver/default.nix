{
  config,
  lib,
  pkgs,
  ...
}: {
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

    xkb.layout = "fr";
  };

  # Support for touchpad
  services.libinput = {
    enable = true;
    touchpad.tapping = false;
  };
}
