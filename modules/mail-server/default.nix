args@{ config, lib, pkgs, ... }:

{
  mailserver = {
    enable = true;
    fqdn = "dwarfmaster.net";
    domains = [ "dwarfmaster.net" ];

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
}
