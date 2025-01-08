{
  config,
  lib,
  pkgs,
  ...
}: {
  home.persistence."/persists/luc".directories = [
    ".cataclysm-dda"
    ".factorio"
  ];
}
