{ config, lib, pkgs, ... }:

{
  imports = [
    (lib.imacs {
      fqdn = "localhost";
      keys-file = ./secrets.sh;
    })
  ];

  services.imacs.enable = true;
}
