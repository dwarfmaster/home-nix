{
  config,
  pkgs,
  lib,
  ...
}: {
  # A minimalistic setup used to bootstrap installation on headless systems
  # like raspberry pis
  # TODO for some reason the SD image is not working
  profiles = {
    users.root.enable = true;
    users.luc-rpi4.enable = true;
  };

  boot.consoleLogLevel = 7;
  sdImage.compressImage = false;
  # Disable persistence
  environment.persistence = lib.mkForce {};
  home-manager.sharedModules = [
    {
      home.persistence = lib.mkForce {};
    }
  ];

  nixpkgs.localSystem.system = "aarch64-linux";
  system.stateVersion = "22.11";
  environment.systemPackages = [
    pkgs.nixos-install-tools
    pkgs.parted
  ];

  powerManagement.cpuFreqGovernor = "ondemand";

  services.openssh.enable = true;
  services.sshd.enable = true;
  programs.mosh.enable = true;

  services.xserver = {
    enable = true;
    desktopManager.xfce.enable = true;
    displayManager.defaultSession = "xfce";
    displayManager.lightdm.enable = true;
  };
}
