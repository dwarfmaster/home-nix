{ config, lib, pkgs, ... }:

{
  # Wallabag installation
  services.nginx.virtualHosts."reading.dwarfmaster.net" = {
    forceSSL = true;
    enableACME = true;

    root = pkgs.wallabag;
    extraConfig = ''
      fastcgi_param WALLABAG_DATA /data/var/www/wallabag;
    '';
    # enablePHP = true;
  };
}
