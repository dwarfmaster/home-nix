{ config, pkgs, lib, ... }:

{
  imports = [
    lib.profiles.core
    lib.hardware.raspberry-pi-4
  ] ++ (builtins.attrValues {
    # Users
    inherit (lib.profiles.users)
      root
      luc-rpi4
    ;

    # Interface
    inherit (lib.profiles.interface)
      sound
      kodi
    ;
  });

  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
  };
  system.stateVersion = "21.11";
  # Enable GPU acceleration
  hardware.raspberry-pi."4".fkms-3d.enable = true;

  powerManagement.cpuFreqGovernor = "ondemand";
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  networking.wireless.enable = false;
  services.openssh.enable = true;
  programs.mosh.enable = true;
}
