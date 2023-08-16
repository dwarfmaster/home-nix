{
  config,
  pkgs,
  lib,
  ...
}: {
  services.korrvigs = {
    enable = true;
    predicates = {
      "title" = [ "entry" "string" ];
      "instance-of" = [ "entry" "entry" ];
      "subclass-of" = [ "entry" "entry" ];
    };
  };
  xsession.windowManager.bspwm.rules = {
    "popup" = {
      center = true;
      focus = true;
      follow = true;
      layer = "above";
      state = "floating";
      rectangle = "1536x864+0+0";
    };
  };
  # Necessary to avoid reinstalling racket packages on each boot
  home.persistence."/persists/luc".directories = [
    "${config.xdg.dataHome}/racket"
  ];
}
