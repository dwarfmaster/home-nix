{
  config,
  lib,
  pkgs,
  ...
}: {
  profiles = {
    users = {
      root.enable = true;
      luc-persist.enable = true;
    };
    system = {
      network.enable = true;
      printing.enable = true;
    };
    interface = {
      xserver.enable = true;
      graphical.enable = true;
      sound.enable = true;
      theme.enable = true;
    };
    services = {
      games.enable = true;
      korrvigs.enable = true;
      # Enable postgresql for playing as user
      sql.enable = true;
    };
  };

  system.stateVersion = "22.05";
  nixpkgs.localSystem.system = "x86_64-linux";

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
      availableKernelModules = ["xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod"];
      kernelModules = [];
    };
    kernelModules = ["kvm-intel" "acpi_call"];
    extraModulePackages = [config.boot.kernelPackages.acpi_call];
    # Unnecessary since / is tmpfs
    tmp.cleanOnBoot = false;

    # Enable aarch64 emulation
    binfmt.emulatedSystems = ["aarch64-linux"];
  };

  powerManagement.cpuFreqGovernor = "powersave";

  # Hardware
  hardware = {
    specs = {
      cores = 4;
      threads = 8;
      kvm = true;
      battery = "BAT0";
      wifiDevice = "wlp0s20f3";
      backlightDevice = "/sys/class/backlight/intel_backlight";
    };
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
      # Necessary system state
      "/var/lib/nixos"
      # Logs
      "/var/log"
      # Systemd specific
      "/var/lib/systemd/coredump"
      # Network manager connections
      "/etc/NetworkManager/system-connections"
      # Postgresql
      {
        directory = "/data/postgresql";
        user = "postgres";
        group = "postgres";
        mode = "0700";
      }
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
      options = ["defaults" "size=16G" "mode=755"];
    };

    "/home/luc" = {
      device = "none";
      fsType = "tmpfs";
      options = ["defaults" "size=4G" "mode=777"];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/CC82-9206";
      fsType = "vfat";
    };

    "/nix" = {
      device = "/dev/disk/by-uuid/04a48731-3ecd-4198-857c-718f10d126de";
      fsType = "f2fs";
      options = ["noatime"];
    };

    "/persists" = {
      device = "/dev/disk/by-uuid/4df4fca6-7c81-4848-bf1a-9c422a95e3ac";
      fsType = "btrfs";
      neededForBoot = true;
      encrypted = {
        enable = true;
        label = "data";
        blkDev = "/dev/disk/by-uuid/b26108e1-3105-4f6b-bffc-eaada861ce62";
      };
    };
  };
  swapDevices = [];

  # Postgresql
  services.postgresql.dataDir =
    let schema = config.services.postgresql.package.psqlSchema;
    in lib.mkForce "/persists/var/lib/postgresql/${schema}";
}
