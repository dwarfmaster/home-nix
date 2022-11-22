{
  config,
  lib,
  pkgs,
  ...
}:
# TODO change elfeed database location in accordance to xdg
{
  programs.doom-emacs.config = {
    modules.dwarfmaster.feeds = {
      config.source = ./config.el;
    };
    initModules.app = [
      {
        mod = "rss";
        args = ["org"];
      }
    ];
  };
}
