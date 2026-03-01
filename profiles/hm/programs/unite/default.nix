{
  config,
  lib,
  ...
}: let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = with pkgs; [
    glib.dev
    nss.dev
    libX11.dev
  ];
}
