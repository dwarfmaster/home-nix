args@{ config, lib, pkgs, simple-nixos-mailserver, ... }:

{

  mailserver = {
    enable = true;
    fqdn = "dwarfmaster.net";
    domains = [ "dwarfmaster.net" ];

    loginAccounts = {
      "luc@dwarfmaster.net" = {
        hashedPassword = "";
        catchAll = [
          "dwarfmaster.net"
        ];
      };

      # TODO after I update the DNS to the new server
      # certificateScheme = 3;

      enableImap = true;
      enablePop3 = true;
      # enableImapSsl = true;
      # enablePop3Ssl = true;

      # We don't use server-side sieves, all the filtering will be done client-side
      enableManageSieve = false;
      virusScanning = true;
    };
  };
}
