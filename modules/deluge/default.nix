{ config, lib, pkgs, ... }:

{
  services.deluge = {
    enable = true;
    declarative = true;

    config = {
      download_location = "/data/var/www/torrents/downloads";
      max_upload_speed = "1000.0";
      daemon_port = 58846;
      allow_remote = false;
      move_completed = true;
      move_completed_path = "/data/var/www/torrents/completed";
      torrentfiles_location = "/data/var/www/torrents/torrents";
    };
    authFile = "/data/var/lib/deluge/auth";

    # Only connect through ssh
    openFirewall = false;
    web = {
      enable = true;
      openFirewall = false;
      port = 8112;
    };
  };

  services.nginx.virtualHosts."downloads.dwarfmaster.net" = {
    forceSSL = true;
    enableACME = true;
    root = "/data/var/www/torrents";
  };
}
