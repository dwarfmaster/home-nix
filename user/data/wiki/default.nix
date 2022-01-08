{ config, lib, pkgs, ... }:

{
  programs.doom = {
    initModules = {
      lang = [ { mod = "org"; args = [ "roam2" "pretty" ]; } ];
    };
    modules.dwarfmaster = {
      org-config.config.source = ./org.el;
      org-attach.config.source = ./attach.el;
      org-roam.config.source   = ./roam.el;
    };
  };
}
