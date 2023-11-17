{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [pkgs.jdk11];
}
