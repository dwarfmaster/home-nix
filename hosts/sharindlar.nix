
  { config, pkgs, lib, ... }:

  {
    imports = [
      ../profiles/core
      lib.hardware.raspberry-pi-4

      # Users
      ../profiles/users/root
      ../profiles/users/luc

      # Interface
      ../profiles/interface/xserver
      ../profiles/interface/sound
    ];

    boot.loader.raspberryPi = {
      enable = true;
      version = 4;
    };

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

    # Enable GPU acceleration
    hardware.raspberry-pi."4".fkms-3d.enable = true;

    system.stateVersion = "21.11";
  }
