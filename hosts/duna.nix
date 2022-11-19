{ config, lib, pkgs, ... }:

{
  imports = [
    lib.profiles.core
  ] ++ (builtins.attrValues {
    # Users
    inherit (lib.profiles.users)
      root
      luc-persist
    ;

    # System
    inherit (lib.profiles.system)
      network
      printing
    ;

    # Interface
    inherit (lib.profiles.interface)
      xserver
      graphical
      sound
    ;
  });

  system.stateVersion = "22.05";

  boot = {
    # TODO re-enable latest_hardened
    kernelPackages = pkgs.linuxPackages_latest;
    kernel.sysctl = {
      "kernel.unpriviledged_userns_clone" = 1;
    };

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    initrd = {
        availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" ];
        kernelModules = [ ];
        luks.devices."data".device = "/dev/disk/by-uuid/b26108e1-3105-4f6b-bffc-eaada861ce62";
    };
    kernelModules = [ "kvm-intel" "acpi_call" ];
    extraModulePackages = [ config.boot.kernelPackages.acpi_call ];
    # Unnecessary since / is tmpfs
    cleanTmpDir = false;
  };

  nix.maxJobs = 8;
  powerManagement.cpuFreqGovernor = "powersave";

  # Hardware
  hardware = {
    acpilight.enable = true;
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = config.hardware.enableRedistributableFirmware;
  };
  services.tlp.enable = true;
  services.thermald.enable = true;
  services.fstrim.enable = true;

  # Persistence
  environment.persistence."/persists/system" = {
    hideMounts = true;
    directories = [
      # Logs
      "/var/log"
      # Systemd specific
      "/var/lib/systemd/coredump"
      # Network manager connections
      "/etc/NetworkManager/system-connections"
    ];
    files = [
      # Systemd specific
      "/etc/machine-id"
    ];
  };
  # Necessary for user-specific impermanence
  programs.fuse.userAllowOther = true;

  # Filesystems
  fileSystems = {
    "/" = {
      device = "none";
      fsType = "tmpfs";
      options = [ "defaults" "size=16G" "mode=755" ];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/CC82-9206";
      fsType = "vfat";
    };

    "/nix" = {
      device = "/dev/disk/by-uuid/04a48731-3ecd-4198-857c-718f10d126de";
      fsType = "f2fs";
      options = [ "noatime" ];
    };

    "/persists" = {
      device = "/dev/disk/by-uuid/4df4fca6-7c81-4848-bf1a-9c422a95e3ac";
      fsType = "btrfs";
    };
  };
  swapDevices = [ ];
}
