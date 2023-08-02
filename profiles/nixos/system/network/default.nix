{
  config,
  pkgs,
  lib,
  ...
}: {
  networking = {
    wireless.enable = false; # Enables wireless support via wpa_supplicant
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
