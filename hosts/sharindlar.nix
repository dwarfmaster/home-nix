{
  config,
  pkgs,
  lib,
  hardware,
  ...
}: {
  imports = [hardware.raspberry-pi-4];

  profiles = {
    users.root.enable = true;
    users.luc-rpi4.enable = true;
    interface = {
      sound.enable = true;
      kodi.enable = true;
    };
  };

  nixpkgs.localSystem.system = "aarch64-linux";
  # Disable persistence
  environment.persistence = lib.mkForce {};
  home-manager.sharedModules = [
    {
      home.persistence = lib.mkForce {};
    }
  ];

  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
  };
  boot.loader.generic-extlinux-compatible.enable = false;
  hardware.specs = {
    cores = 4;
    threads = 8;
  };
  system.stateVersion = "22.11";
  # Enable GPU acceleration
  hardware.raspberry-pi."4".fkms-3d.enable = true;

  powerManagement.cpuFreqGovernor = "ondemand";
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = ["noatime"];
    };
  };

  networking.wireless.enable = false;
  services.openssh.enable = true;
  programs.mosh.enable = true;
}
