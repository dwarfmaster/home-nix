args@{ config, lib, pkgs, ... }:

{
  # TODO add assertion that redis doesn't use a unix socket
  mailserver = {
    enable = true;
    fqdn = "dwarfmaster.net";
    domains = [ "dwarfmaster.net" ];
    mailDirectory = "/data/var/vmail";

    loginAccounts = {
      "luc@dwarfmaster.net" = {
        hashedPassword = config.users.users.luc.hashedPassword;
        catchAll = [
          "dwarfmaster.net"
        ];
      };
    };

    certificateScheme = 3;

    enableImap = true;
    enablePop3 = true;
    enableImapSsl = true;
    enablePop3Ssl = true;

    # We don't use server-side sieves, all the filtering will be done client-side
    enableManageSieve = false;
    virusScanning = true;
  };

  assertions = [
    { assertion = isNull config.services.redis.unixSocket;
      message = "simple-nixos-mailserver doesn't support redis over UNIX socket"; }
  ];
}
