{ config, lib, pkgs, simple-nixos-mailserver, ... }:

{
  imports = [
    # Users
    ../users/root
    ../users/luc-server

    # Basic configuration
    ../modules/nix
    ../modules/locale
    ../modules/common-env

    # Services
    ../modules/mail-server
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest_hardened;
    kernel.sysctl = {
      "kernel.unprivileged_userns_clone" = 1;
    };
    kernelParams = [ "ip=188.165.216.157" ];

    initrd = {
      availableKernelModules = [ "uhci_hcd" "ahci" "usbhid" "e1000e" ];
      kernelModules = [ ];
      luks.devices.data.device = "/dev/disk/by-label/515e7b79-ce78-4666-a30a-8ad1150d0810";
      network = {
        enable = true;
        ssh = {
          enable = true;
          port = 6922;
          authorizedKeys = with lib; concatLists (mapAttrsToList
            (name: user: if elem "wheel" user.extraGroups
                         then user.openssh.authorizedKeys.keys
                         else [])
            config.users.users);
          hostKeys = [
            "/var/host/ecdsa"
            "/var/host/rsa"
            "/var/host/ed25519"
          ];
        };
      };
      # postMountCommands = ''
      #   for int in /sys/class/net/*/
      #     do ip link set `basename $int` down
      #   down
      # '';
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];

    loader.grub = {
      enable = true;
      version = 2;
      efiSupport = false;
      device = "/dev/sda";
    };
  };
  system.stateVersion = "20.09";

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  # Filesystems
  environment.systemPackages = with pkgs; [
    cryptsetup
  ];
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/cf566959-5090-4121-b268-df3e581ca2bd";
      fsType = "ext4";
    };

    "/data" = {
      device = "/dev/mapper/data";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/ca63c5b4-3ec5-4338-9ff7-54fe935b83d0";
      fsType = "ext4";
    };
  };

  # Networking
  networking.wireless.enable = false;
  networking.useDHCP = false;
  networking.usePredictableInterfaceNames = true;
  networking.defaultGateway = "188.165.216.254";
  networking.nameservers = [ "213.186.33.99" "1.1.1.1" ];
  networking.interfaces.enp1s0.ipv4.addresses = [ {
    address = "188.165.216.157";
    prefixLength = 24;
  } ];
  networking.defaultGateway6 = {
    address = "2001:41d0:2:a3ff:ff:ff:ff:ff";
    interface = "enp1s0";
  };
  networking.interfaces.enp1s0.ipv6.addresses = [ {
    address = "2001:41d0:2:a39d::1";
    prefixLength = 128;
  } ];
  networking.firewall.allowPing = true;
  networking.firewall.pingLimit = "--limit 10/minute --limit-burst 50";

  # NGinx
  security.acme = {
    acceptTerms = true;
    email = "acme@dwarfmaster.net";
  };

  services.nginx = {
    enable = true;
    virtualHosts = {
      "dwarfmaster.net" = {
        forceSSL = true;
        enableACME = true;
        serverAliases = [ "blog.dwarfmaster.net" "www.dwarfmaster.net" ];
        locations."/" = {
          root = "/var/www/blog";
        };
      };

      "info16.dwarfmaster.net" = {
        forceSSL = true;
        enableACME = true;
        globalRedirect = "info16.twal.org";
      };

      "assofrancojap.dwarfmaster.net" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          root = "/var/www/assofrancojap";
        };
      };
    };
  };
  systemd.services.nginx.serviceConfig.ReadWritePaths = [ "/var/www/" ];
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  # SSH
  services.openssh = {
    enable = true;
    banner = ''
 ____                      __ __  __           _                       _
|  _ \__      ____ _ _ __ / _|  \/  | __ _ ___| |_ ___ _ __ _ __   ___| |_
| | | \ \ /\ / / _` | '__| |_| |\/| |/ _` / __| __/ _ \ '__| '_ \ / _ \ __|
| |_| |\ V  V / (_| | |  |  _| |  | | (_| \__ \ ||  __/ | _| | | |  __/ |_
|____/  \_/\_/ \__,_|_|  |_| |_|  |_|\__,_|___/\__\___|_|(_)_| |_|\___|\__|

Welcome !
'';
    forwardX11 = false;
    logLevel = "VERBOSE";
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
    permitRootLogin = "no";
    ports = [ 2222 ];
  };
  programs.mosh.enable = true;
}
