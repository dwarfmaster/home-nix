{ config, lib, pkgs, ... }:

{
  services.nginx.virtualHosts."element.dwarfmaster.net" = {
    enableACME = true;
    forceSSL = true;
    serverAliases = [
      "matrix.dwarfmaster.net"
    ];

    root = pkgs.element-web.override {
      conf = {
        default_server_config."m.homeserver" = {
          "base_url" = "dwarfmaster.net";
          "server_name" = "synapse.dwarfmaster.net";
        };
      };
    };
  };
}
