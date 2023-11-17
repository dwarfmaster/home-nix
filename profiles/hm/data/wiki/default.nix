{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.hugo # Generate the web wiki
  ];
}
