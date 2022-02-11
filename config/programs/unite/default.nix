{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = with pkgs; [
    glib.dev
    nss.dev
    xorg.libX11.dev
  ];
}
