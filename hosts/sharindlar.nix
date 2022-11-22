{ config, pkgs, lib, ... }:

{
  # TODO will fail with infinite recursion
  imports = [ config.lib.hardware.raspberry-pi-4 ];

  profiles = {
    users.root.enable = true;
    interface = {
      sound.enable = true;
      kodi.enable = true;
    };
  };

  nixpkgs = {
    localSystem = "x86_64-linux";
    crossSystem = "aarch64-linux";
  };

  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
  };
  hardware.specs = {
    cores = 4;
    threads = 8;
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
