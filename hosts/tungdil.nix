{
  config,
  lib,
  pkgs,
  ...
}: {
  profiles = {
    users = {
      root.enable = true;
      luc.enable = true;
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
      # grafana.enable = true;
    };
  };

  system.stateVersion = "21.11";
  nixpkgs.localSystem.system = "x86_64-linux";
  # Disable persistence
  environment.persistence = lib.mkForce {};
  home-manager.sharedModules = [{
    home.persistence = lib.mkForce {};
  }];

  boot = {
    # TODO re-enable latest_hardened
    # There is a bug on latest regarding the screen
    # kernelPackages = pkgs.linuxPackages_latest;
    kernel.sysctl = {
      "kernel.unprivileged_userns_clone" = 1;
    };

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    initrd = {
      availableKernelModules = ["xhci_pci" "nvme" "usb_storage" "sd_mod" "i915" "intel_agp"];
      kernelModules = [];
    };
    kernelModules = ["kvm-intel" "thunderbolt"];
    blacklistedKernelModules = ["nouveau"];
    extraModulePackages = [];
    tmp.cleanOnBoot = true;
  };
  hardware = {
    specs = {
      cores = 6;
      threads = 12;
      kvm = true;
      battery = "BAT1";
      wifiDevice = "wlo1";
      backlightDevice = "/sys/class/backlight/intel_backlight";
    };
    enableRedistributableFirmware = true;
  };

  powerManagement.cpuFreqGovernor = "powersave";

  systemd.extraConfig = ''
    DefaultTimeoutStopSec = 10s
  '';

  # # Graphic card
  services.xserver.videoDrivers = ["i915" "modesetting"];
  hardware.opengl = {
    enable = true;
    driSupport = true;
  };
  hardware.acpilight.enable = true;

  # RGB driver for keyboard
  hardware.ckb-next.enable = true;

  # Filesystems
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/1be3caa1-f5df-4e19-831e-9add0f8fed1b";
      fsType = "ext4";
    };

    "/data" = {
      device = "/dev/disk/by-uuid/f9dcf866-2a61-4823-826f-e27250197f9f";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/9366-2848";
      fsType = "vfat";
    };
  };

  # Network
  # systemd.network = {
  #   networks = {
  #     "40-wired" = {
  #       enable = true;
  #       matchConfig.Name = "enp61s0";
  #       networkConfig.DHCP = "yes";
  #       dhcpConfig.RouteMetric = 10;
  #     };
  #     "40-wireless" = {
  #       enable = true;
  #       matchConfig.Name = "wlo1";
  #       networkConfig.DHCP = "yes";
  #       dhcpConfig.RouteMetric = 10;
  #     };
  #   };
  # };
}
