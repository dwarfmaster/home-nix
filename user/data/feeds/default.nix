{ config, lib, pkgs, ... }:

{
  programs.doom = {
    modules.dwarfmaster.feeds = {
      config.source = ./config.el;
    };
    initModules.app = [
      { mod = "rss"; args = [ "org" ]; }
    ];
  };
}
