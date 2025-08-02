args @ {
  config,
  lib,
  pkgs,
  ...
}: {
  mailserver = {
    enable = true;
    stateVersion = 3;
    fqdn = "dwarfmaster.net";
    domains = ["dwarfmaster.net"];
    mailDirectory = "/data/var/vmail";

    loginAccounts = {
      "luc@dwarfmaster.net" = {
        hashedPassword = config.users.users.luc.hashedPassword;
        catchAll = [
          "dwarfmaster.net"
        ];
      };
    };

    certificateScheme = "acme-nginx";

    enableImap = true;
    enablePop3 = true;
    enableImapSsl = true;
    enablePop3Ssl = true;

    # We don't use server-side sieves, all the filtering will be done client-side
    enableManageSieve = false;
    virusScanning = true;
  };
}
