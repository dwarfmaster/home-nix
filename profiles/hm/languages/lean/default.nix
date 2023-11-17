{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [pkgs.lean];
}
