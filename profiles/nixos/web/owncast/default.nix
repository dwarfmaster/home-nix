{ pkgs, config, ... }:

let
  owncast = config.services.owncast;
in{
  services.owncast = {
    enable = true;
    dataDir = "/data/var/owncast";
    port = 8081;
    openFirewall = true;
  };

  services.nginx.virtualHosts."owncast.dwarfmaster.net" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${builtins.toString owncast.port}";
      recommendedProxySettings = true;
    };
  };
}
