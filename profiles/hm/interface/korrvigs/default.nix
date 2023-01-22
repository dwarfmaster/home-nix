{ config, pkgs, ... }:

{
  services.korrvigs = {
    constants.term = "${config.applications.terminal}";
    extraModules.popup = ./popup.pl;
  };
  xsession.windowManager.bspwm.rules = {
    "popup" = {
      center = true;
      focus = true;
      follow = true;
      layer = "above";
      state = "floating";
      geometry = "1536x864+0+0";
    };
  };
}
