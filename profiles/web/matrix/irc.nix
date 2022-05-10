{ config, lib, pkgs, ... }:

let
  server = config.services.matrix-synapse;
  listener = builtins.head server.listeners;
  cfg = config.services.matrix-appservice-irc;
  registrationFile = "/var/lib/heisenbridge/registration.yml";
  bin = "${pkgs.unstable.heisenbridge}/bin/heisenbridge";
  compat = !config.services.matrix-synapse.enable;
  genopt = "--generate${if compat then "-compat" else ""}";
in {
  # Generates /var/lib/matrix-appservice-irc/registration.yml on first run
  services.matrix-appservice-irc = {
    enable = false;
    registrationUrl = "http://localhost:${toString cfg.settings.port}";
    needBindingCap = true;

    settings = {
      homeserver.url = "http://localhost:${toString listener.port}";
      homeserver.domain = "dwarfmaster.net";
      port = 8009;

      ircService.servers."ulminfo.fr" = {
        name = "ulm";
        port = 6666;
        ssl  = true;
        dynamicChannels = {
          enabled = true;
          aliasTemplate = "#ulmirc_$CHANNEL";
          groupId = "+ulmirc:localhost";
          joinRule = "public";
          federate = false;
        };
        matrixClients = {
          userTemplate = "@ulm_$NICK";
        };
        ircClients = {
          nickTemplate = "$LOCALPART[m]";
          allowNickChanges = false;
        };
      };
    };
  };

  systemd.services.heisenbridge = {
    description = "Simple Matrix-IRC bridge";
    before = [ "matrix-synapse.service" ];
    wantedBy = [ "multi-user.target" ];

    # Generates /var/lib/heisenbridge/registration.yml that must be copied to somewhere matrix-synapse has access
    preStart = ''
      umask 077
      # Inspired by the code for appservice-irc, prevent changing the keys when the config is updated
      if ! [ -f "${registrationFile}" ]; then
        ${bin} --config ${registrationFile} ${genopt} -l localhost -p 8009 http://localhost:${toString listener.port}
      else
        # 1. Backup
        hs_token=$(grep "^hs_token:.*$" ${registrationFile})
        as_token=$(grep "^as_token:.*$" ${registrationFile})
        # 2. Regenerate
        rm ${registrationFile}
        ${bin} ${genopt} --config ${registrationFile} -l localhost -p 8009 http://localhost:${toString listener.port}
        # 3. Restore
        sed -i "s/^hs_token:.*$/$hs_token/g" ${registrationFile}
        sed -i "s/^as_token:.*$/$as_token/g" ${registrationFile}
      fi
    '';

    serviceConfig = {
      Type = "simple";
      ExecStart = "${bin} --config ${registrationFile} -l localhost -p 8009 http://localhost:${toString listener.port}";

      User = "matrix-heisenbridge";
      Group = "matrix-heisenbridge";
      StateDirectory = "heisenbridge";
      StateDirectoryMode = "755";
    };
  };

  users.groups.matrix-heisenbridge = {};
  users.users.matrix-heisenbridge = {
    description = "Service user for the Matrix-IRC bridge";
    group = "matrix-heisenbridge";
    isSystemUser = true;
  };
}
