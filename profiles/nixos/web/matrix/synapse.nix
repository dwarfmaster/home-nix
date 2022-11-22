{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.matrix-synapse;
  fqdn = "synapse.dwarfmaster.net";
  # SQL script to create the database before first launch
  init-script = pkgs.writeText "synapse-init.sql" ''
    CREATE ROLE "matrix-synapse" WITH LOGIN PASSWORD 'synapse';
    CREATE DATABASE "matrix-synapse" WITH OWNER "matrix-synapse"
      TEMPLATE template0
      LC_COLLATE = "C"
      LC_CTYPE = "C";
  '';
  get-init-script = pkgs.writeScriptBin "matrix-synapse-get-init" ''
    echo ${init-script}
  '';
  irc = config.services.heisenbridge;
in {
  # Database
  services.postgresql.enable = true;
  # Must be manually setup

  # Nginx host
  services.nginx.virtualHosts."dwarfmaster.net" = {
    enableACME = true;
    forceSSL = true;
    locations."= /.well-known/matrix/server".extraConfig = let
      server = {"m.server" = "${fqdn}:443";};
    in ''
      add_header Content-Type application/json;
      return 200 '${builtins.toJSON server}';
    '';
    locations."= /.well-known/matrix/client".extraConfig = let
      client = {
        "m.homeserver" = {"base_url" = "https://${fqdn}";};
        "m.identity_server" = {"base_url" = "https://vector.im";};
      };
    in ''
      add_header Content-Type application/json;
      add_header Access-Control-Allow-Origin *;
      return 200 '${builtins.toJSON client}';
    '';
  };
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

  # Synapse configuration
  # To create the root user, set registration_shared_secret to a value, then run:
  # register_new_matrix_user -k your-registration-shared-secret http://localhost:8008
  services.matrix-synapse = {
    enable = true;
    settings = {
      server_name = "dwarfmaster.net";
      enable_registration = false;
      app_service_config_files =
        lib.optionals irc.enable ["/var/lib/matrix-synapse/irc-registration.yml"];
      listeners = [
        {
          port = 8008;
          bind_addresses = ["::1"];
          type = "http";
          tls = false;
          x_forwarded = true;
          resources = [
            {
              names = ["client" "federation"];
              compress = false;
            }
          ];
        }
      ];
    };
  };

  # Install tools
  environment.systemPackages = [pkgs.matrix-synapse get-init-script];
}
