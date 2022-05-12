
  { config, pkgs, lib, ... }:

  {
    imports = [
      lib.profiles.core
      lib.hardware.raspberry-pi-4
    ] ++ (builtins.attrValues {
      # Users
      inherit (lib.profiles.users)
        root
        luc
      ;

      # Interface
      inherit (lib.profiles.interface)
        xserver
        sound
      ;
    });

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
