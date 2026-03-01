{
  config,
  pkgs,
  lib,
  ...
}: {
  networking = {
    useDHCP = false;
    networkmanager.enable = true;
    extraHosts = ''
      127.0.0.1 9gag.com www.9gag.com
    '';
  };

  services.ntp.enable = true;
  services.openssh = {
    enable = true;
    settings.X11Forwarding = true;
  };
  services.gvfs.enable = true;
}
