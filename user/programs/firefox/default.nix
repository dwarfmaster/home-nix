{ config, lib, pkgs, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    extensions = builtins.attrValues {
      inherit (pkgs.nur.rycee.firefox-addons) ublock-origin;
    };
  };
}
