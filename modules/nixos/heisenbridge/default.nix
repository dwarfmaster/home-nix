{ config, lib, pkgs, ... }:

let
  inherit (lib)
    mkEnableOption mkOption types mkIf;
  cfg = config.services.heisenbridge;
  registrationFile = "/var/lib/heisenbridge/registration.yml";
  bin    = "${cfg.package}/bin/heisenbridge";
  genopt = "--generate${if cfg.compat then "-compat" else ""}";
  opts   = "--config ${registrationFile} -l localhost -p ${toString cfg.port} ${cfg.homeserver}";
in {
  options = {
    services.heisenbridge = {
      enable = mkEnableOption "Enable Heisenbridge Matrix<->IRC bridge";
      port = mkOption {
        description = "The port the bridge is support to listen on";
        type = types.port;
        default = 8009;
      };
      compat = mkOption {
        description = "Should the registration.yml file be generated for other homeserver than synapse";
        type = types.bool;
        default = !config.services.matrix-synapse.enable;
      };
      homeserver = mkOption {
        description = "The url of the homeserver";
        type = types.str;
        default = "http://localhost:8008";
      };
      user = mkOption {
        description = "The user heisenbridge will be run as";
        type = types.str;
        default = "matrix-heisenbridge";
      };
      group = mkOption {
        description = "The group heisenbridge will be run as";
        type = types.str;
        default = cfg.user;
      };
      package = mkOption {
        description = "The heisenbridge app";
        type = types.package;
        default = pkgs.heisenbridge;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.heisenbridge = {
      description = "Simple Matrix-IRC bridge";
      before = [ "matrix-synapse.service" ];
      wantedBy = [ "multi-user.target" ];

      # Generates /var/lib/heisenbridge/registration.yml that must be copied to somewhere matrix-synapse has access
      preStart = ''
        # Inspired by the code for appservice-irc, prevent changing the keys when the config is updated
        if ! [ -f "${registrationFile}" ]; then
          ${bin} ${genopt} ${opts}
        else
          # 1. Backup
          hs_token=$(grep "^hs_token:.*$" ${registrationFile})
          as_token=$(grep "^as_token:.*$" ${registrationFile})
          # 2. Regenerate
          rm ${registrationFile}
          ${bin} ${genopt} ${opts}
          # 3. Restore
          sed -i "s/^hs_token:.*$/$hs_token/g" ${registrationFile}
          sed -i "s/^as_token:.*$/$as_token/g" ${registrationFile}
        fi
      '';

      serviceConfig = let
        capabilities = if cfg.port < 1024 then [ "CAP_NET_BIND_SERVICE" ] else [ "" ];
      in {
        Type      = "simple";
        ExecStart = "${bin} ${opts}";

        User = "${cfg.user}";
        Group = "${cfg.group}";
        UMask = "077";
        StateDirectory = "heisenbridge";
        StateDirectoryMode = "755";
        ReadWritePaths = [ "/var/lib/heisenbridge" ];

        PrivateDevices = true;
        PrivateMounts = true;
        PrivateTmp = true;
        PrivateUsers = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        ProtectHostname = true;
        ProtectClock = true;
        ProtectProc = "invisible";
        ProcSubset = "pid";
        DevicePolicy = "closed";

        CapabilityBoundingSet = capabilities;
        AmbientCapabilities = capabilities;
        NoNewPrivileges = true;

        LockPersonality = true;
        RestrictRealtime = true;
        RestrictNamespaces = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        SystemCallErrorNumber = "EPERM";
        SystemCallFilter = [ "@system-service" "~@resources" "~@privileged" ];
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];
        RemoveIPC = true;
      };
    };

    users.groups."${cfg.group}" = {};
    users.users."${cfg.user}" = {
      description = "Service user for the Matrix-IRC bridge";
      group = "${cfg.group}";
      isSystemUser = true;
    };
  };
}
