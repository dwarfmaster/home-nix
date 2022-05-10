{ config, lib, pkgs, ... }:

{
  # TODO https://github.com/vector-im/element-web#important-security-notes
  services.nginx.virtualHosts."element.matrix.dwarfmaster.net" = {
    enableACME = true;
    forceSSL = true;

    root = pkgs.element-web.override {
      conf = {
        default_server_config."m.homeserver" = {
          "base_url" = "https://synapse.dwarfmaster.net";
          "server_name" = "synapse.dwarfmaster.net";
        };
      };
    };
  };
}
