{ config, lib, pkgs, ... }:

{
  programs.doom-emacs.config = {
    modules.dwarfmaster.feeds = {
      config.source = ./config.el;
    };
    initModules.app = [
      { mod = "rss"; args = [ "org" ]; }
    ];
  };
}
