{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.cataclysm-dda
  ];
}
