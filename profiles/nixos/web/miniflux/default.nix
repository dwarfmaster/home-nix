{ pkgs, config, ... }:

let
  url = "rss.dwarfmaster.net";
  cfg = config.services.miniflux;
in {
  services.miniflux = {
    enable = true;
    config = {
      FETCH_YOUTUBE_WATCH_TIME = "1";
      WORKER_POOL_SIZE = "4";
      POLLING_FREQUENCY = "240";
      BASE_URL = "https://${url}";
      CLEANUP_ARCHIVE_UNREAD_DAYS = "360";
      CLEANUP_ARCHIVE_READ_DAYS = "360";
    };
    # Must be set manually
    adminCredentialsFile = "/etc/nixos/miniflux-admin-credentials";
  };

  services.nginx.virtualHosts."${url}" = {
    forceSSL = true;
    enableACME = true;
    locations."/".proxyPass = "http://${cfg.config.LISTEN_ADDR}";
  };
}
