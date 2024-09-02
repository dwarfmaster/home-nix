{
  config,
  lib,
  pkgs,
  ...
}: let
  server = config.services.matrix-synapse;
  listener = builtins.head server.settings.listeners;
  cfg = config.services.heisenbridge;
in {
  # Generates /var/lib/heisenbridge/registration.yml on first run that must be
  # copied to somewhere matrix-synapse has access
  services.heisenbridge = {
    enable = true;
    port = 8009;
    homeserver = "http://localhost:${toString listener.port}";
    package = pkgs.stable.heisenbridge;
  };
}
