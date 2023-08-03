{
  config,
  lib,
  pkgs,
  simple-nixos-mailserver,
  ...
}: {
  profiles = {
    users = {
      root.enable = true;
      luc-server.enable = true;
    };
    services = {
      mail-server.enable = true;
      torrent.enable = true;
    };
    web = {
      matrix.enable = true;
      nextcloud.enable = true;
      imacs.enable = false;
      wallabag.enable = false;
      grocy.enable = false;
      arkenfox.enable = true;
      pixelfed.enable = false;
      miniflux.enable = true;
    };
    interface = {
      theme.enable = true;
    };
  };

  nixpkgs.localSystem.system = "x86_64-linux";

  boot = {
    # TODO re-enable latest_hardened
    kernelPackages = pkgs.linuxPackages_latest;
    kernel.sysctl = {
      "kernel.unprivileged_userns_clone" = 1;
    };

    initrd = {
      availableKernelModules = ["uhci_hcd" "ahci" "usbhid"];
      kernelModules = [];
    };
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];

    loader.grub = {
      enable = true;
      version = 2;
      efiSupport = false;
      device = "/dev/sda";
    };
  };
  system.stateVersion = "21.05";
  hardware.specs = {
    cores = 4;
    threads = 8;
    kvm = true;
  };

  powerManagement.cpuFreqGovernor = "ondemand";

  # Filesystems
  environment.systemPackages = with pkgs; [
    cryptsetup
    apacheHttpd # For htpasswd
  ];
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/26dd438a-3009-43f3-9796-a88568e92fb5";
      fsType = "ext4";
    };

    # I must first luksOpen the right disk manually and then mount it by hand
    "/data" = {
      device = "/dev/mapper/data";
      fsType = "ext4";
      options = ["defaults" "noauto"];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/c3075475-8a0a-4438-839f-4ee048e083ce";
      fsType = "ext4";
    };
  };

  # Networking
  networking.defaultGateway = "188.165.216.254";
  networking.nameservers = ["213.186.33.99" "1.1.1.1"];
  networking.interfaces.enp1s0.ipv4.addresses = [
    {
      address = "188.165.216.157";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway6 = {
    address = "2001:41d0:2:a3ff:ff:ff:ff:ff";
    interface = "enp1s0";
  };
  networking.interfaces.enp1s0.ipv6.addresses = [
    {
      address = "2001:41d0:2:a39d::1";
      prefixLength = 128;
    }
  ];
  networking.firewall.allowPing = true;
  networking.firewall.pingLimit = "--limit 10/minute --limit-burst 50";

  # NGinx
  security.acme = {
    acceptTerms = true;
    defaults.email = "acme@dwarfmaster.net";
  };

  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;

    virtualHosts = {
      "dwarfmaster.net" = {
        forceSSL = true;
        enableACME = true;
        serverAliases = ["blog.dwarfmaster.net" "www.dwarfmaster.net"];
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
          index = "SiteAccueil.html";
        };
      };
    };
  };
  systemd.services.nginx.serviceConfig.ReadWritePaths = ["/var/www/"];
  networking.firewall.allowedTCPPorts = [80 443];

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
    settings.X11Forwarding = false;
    logLevel = "VERBOSE";
    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
    permitRootLogin = "no";
    ports = [2222];
  };
  programs.mosh.enable = true;

  # PostgreSQL
  services.postgresql = {
    enable = true;
    dataDir = "/data/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
  };
}
