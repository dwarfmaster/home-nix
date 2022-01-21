{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  programs.doom-emacs.config = {
    initModules = {
      lang = [ { mod = "org"; args = [ "roam2" "pretty" "hugo" ]; } ];
    };
    modules.dwarfmaster = {
      org-config.config.source = ./org.el;
      org-attach.config.source = ./attach.el;
      org-roam.config.source   = ./roam.el;
      org-links.config.source  = ./links.el;
    };
  };

  home.packages = [
    pkgs.hugo # Generate the web wiki
  ];
}
