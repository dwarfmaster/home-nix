{ config, lib, pkgs, ... }:

{
  programs.doom = {
    initModules = {
      lang = [ { mod = "org"; args = [ "roam" "pretty" ]; } ];
    };
    modules.dwarfmaster = {
      org-config.config.source = ./org.el;
      org-attach.config.source = ./attach.el;
      org-roam.config.source   = ./roam.el;
    };
  };
}
