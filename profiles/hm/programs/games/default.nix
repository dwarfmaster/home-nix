{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.cataclysm-dda
  ];

  home.persistence."/persists/luc".directories = [
    ".cataclysm-dda"
    ".factorio"
    ".steam"
    ".local/share/Steam"
  ];
}
