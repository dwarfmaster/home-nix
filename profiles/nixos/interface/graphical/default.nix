{
  config,
  lib,
  pkgs,
  ...
}:
# Generic options for  all graphical-enabled machines
{
  # Necessary for most of gtk configuration as user
  programs.dconf.enable = true;
}
