{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = [ pkgs.lean ];
  programs.doom-emacs.config.initModules.lang = [ "lean" ];
}
