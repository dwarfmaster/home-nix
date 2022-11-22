{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.openconnect
    pkgs.networkmanager-openconnect
  ];
}
