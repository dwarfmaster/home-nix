{
  config,
  lib,
  pkgs,
  ...
}:
# Generic options for  all graphical-enabled machines
{
  # Add dconf as dbus service
  services.dbus.packages = [pkgs.dconf];
}
