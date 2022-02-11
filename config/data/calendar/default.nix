{ config, lib, pkgs, ... }:

{
  programs.doom-emacs.config = {
    modules.apps.caldav = {
      config.source = ./config.el;
      packages.text = ''
        (package! org-caldav)
      '';
      nix = {
        xdg-cache = "${config.xdg.cacheHome}";
      };
    };
  };
}
