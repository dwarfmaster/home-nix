{ config, lib, pkgs, ... }:

let
  fqdn = "synapse.dwarfmaster.net";
in {
  # Synapse configuration
  services.matrix-synapse = {
    enable = true;
    server_name = "dwarfmaster.net";
    enable_registration = false;
    listeners = [
      {
        port = 8008;
        bind_address = "::1";
        type = "http";
        tls = false;
        x_forwarded = true;
        resources = [
          {
            names = [ "client" "federation" ];
            compress = false;
          }
        ];
      }
    ];
  };

  # Nginx host
  services.nginx.virtualHosts.${fqdn} = {
    enableACME = true;
    forceSSL = true;
    locations."/".extraConfig = ''
      return 404;
    '';

    locations."/_matrix" = {
      proxyPass = "http://[::1]:8008";
    };
  };

  # Database
  # Postgresql must be running, the setup is manual

  # Install tools
  environment.systemPackages = [ pkgs.matrix-synapse ];
}
