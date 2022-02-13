{ config, lib, pkgs, ... }:

{
  # NGinx setup
  services.nginx.virtualHosts."nextcloud.dwarfmaster.net" = {
    forceSSL = true;
    enableACME = true;
  };

  # Nextcloud
  services.nextcloud = {
    enable = true;
    home = "/data/var/lib/nextcloud";
    hostName = "nextcloud.dwarfmaster.net";
    # Use https
    https = true;

    package = pkgs.nextcloud22;

    # Auto-update Nextcloud apps
    autoUpdateApps.enable = true;
    # Set auto-update time
    autoUpdateApps.startAt = "05:00:00";

    config = {
      # Further forces https
      overwriteProtocol = "https";

      # PostgreSQL configuration
      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql";
      dbname = "nextcloud";
      dbpassFile = "/data/var/nextcloud/db-pass";

      adminpassFile = "/data/var/nextcloud/admin-pass";
      adminuser = "admin";
    };
  };

  # Database configuration
  services.postgresql = {
    # Ensure the database, user and permission always exist
    ensureDatabases = [ "nextcloud" ];
    ensureUsers = [
      { name = "nextcloud";
        ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
      }
    ];
  };

  # Make sure postgresql is running before nextcloud
  systemd.services."nextcloud-setup" = {
    requires = [ "postgresql.service" ];
    after = [ "postgresql.service" ];
  };
}
