{ config, lib, pkgs, ... }:

{
  services = {
    printing.enable = true;
    avahi.enable = true;
    samba.enable = true;
  };
}
