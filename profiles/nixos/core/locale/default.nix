{ config, pkgs, lib, ... }:

{
  i18n.defaultLocale = "en_IE.UTF-8";
  time.timeZone = "Europe/Paris";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "fr";
  };
}
