{ config, lib, pkgs, ... }:

{
  services.imacs = {
    enable = true;
    hostName = "imacs.dwarfmaster.net";
    # TODO manage secrets
    keysFile = "/root/imacs-secrets.sh";
    unsafeSettings = false;
  };

  services.nginx.virtualHosts."imacs.dwarfmaster.net".enableACME = true;
}
