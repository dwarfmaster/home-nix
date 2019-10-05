
let

  mlib = import ../lib/lib.nix;

in {


  #  _____                      _ _ _ 
  # |_   _|   _ _ __   __ _  __| (_) |
  #   | || | | | '_ \ / _` |/ _` | | |
  #   | || |_| | | | | (_| | (_| | | |
  #   |_| \__,_|_| |_|\__, |\__,_|_|_|
  #                   |___/           
  # MSI GS65 laptop
  tungdil =
    { config, lib, pkgs, ... }:
    { deployment.targetHost = "127.0.0.1";

      boot = {
        initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
        initrd.kernelModules = [ ];
        kernelModules = [ "kvm-intel" "thunderbold" ];
        blacklistedKernelModules = [ "nouveau" ];
        extraModulePackages = [ ];
        loader.systemd-boot.enable = true;
        loader.efi.canTouchEfiVariables = true;
      };

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

      swapDevices = [
        { device = "/dev/disk/by-uuid/8ba2256c-d6ec-4270-8806-5ecfe845adb8"; }
      ];

      hardware = {
        # TODO make NVIDIA card work along intel card
        nvidiaOptimus.disable = true;
        opengl.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
        opengl.extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];

        # Control corsair devices leds
        ckb-next.enable = true;
      };

      powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
      nix.maxJobs = lib.mkDefault 12;
    };



  #  ____        _           _ _ _ 
  # | __ )  ___ (_)_ __   __| (_) |
  # |  _ \ / _ \| | '_ \ / _` | | |
  # | |_) | (_) | | | | | (_| | | |
  # |____/ \___/|_|_| |_|\__,_|_|_|
  #
  # Tower computer
  boindil =
    { config, lib, pkgs, ... }:
    { deployment.targetHost = "77.141.116.119";

      boot = {
        initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
        initrd.luks.devices = [ { name = "luksdata"; device = "/dev/sdb1"; } ];
        kernelModules = [ "kvm-intel" "pcspkr" ];
        extraModulePackages = [ ];
        loader.systemd-boot.enable = true;
        loader.efi.canTouchEfiVariables = true;
      };

      fileSystems = {
        "/" =  {
          device = "/dev/disk/by-label/mainos";
          fsType = "ext4";
        };
        "/boot" = {
          device = "/dev/disk/by-label/boot";
        };
        "/home/luc/data" = {
          device = "/dev/mapper/luksdata";
        };
      };

      swapDevices = [
        { device = "/dev/disk/by-label/swap"; }
      ];

      nix.maxJobs = lib.mkDefault 8;
    };

}

