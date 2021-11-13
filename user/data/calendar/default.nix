{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  programs.doom = {
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
