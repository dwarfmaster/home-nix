{
  config,
  lib,
  pkgs,
  ...
}: {
  profiles = {
    users = {
      root.enable = true;
      luc-wsl.enable = true;
    };
    system = {
      network.enable = true;
      printing.enable = false;
    };
    interface = {
      xserver.enable = false;
      graphical.enable = false;
      sound.enable = false;
      theme.enable = true;
    };
    services = {
      games.enable = false;
      korrvigs.enable = true;
      sql.enable = true;
    };
  };

  wsl.enable = true;
  wsl.defaultUser = "luc";
  system.stateVersion = "24.11";
  nixpkgs.localSystem.system = "x86_64-linux";

  hardware = {
    specs = {
      cores = 8;
      threads = 12;
      kvm = false;
    };
  };

  # Disable persistence
  environment.persistence = lib.mkForce {};
  home-manager.sharedModules = [
    {
      home.persistence = lib.mkForce {};
    }
  ];
}
