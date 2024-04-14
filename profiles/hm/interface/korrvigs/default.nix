{
  config,
  pkgs,
  lib,
  ...
}: {
  services.korrvigs = {
    enable = false;
    wikiDir = "/home/luc/downloads/wiki";
    # The rels.json and rules.json files are exported by the running korrvigs instance
    predicates = builtins.fromJSON (builtins.readFile ./rels.json);
    rules = builtins.fromJSON (builtins.readFile ./rules.json);
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
