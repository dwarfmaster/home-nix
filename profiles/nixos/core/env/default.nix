{
  config,
  lib,
  pkgs,
  ...
}: let
  system =
    if config.nixpkgs.crossSystem ? system
    then config.nixpkgs.crossSystem.system
    else config.nixpkgs.localSystem.system;
in {
  environment.systemPackages = with pkgs; [
    binutils
    coreutils
    curl
    direnv
    dnsutils
    dosfstools
    exfat
    fd
    git
    gnufdisk
    gotop
    gptfdisk
    htop
    iputils
    jq
    lm_sensors
    moreutils
    nmap
    ntfs3g
    ripgrep
    utillinux
    vim
    wget
    whois
  ];

  programs.adb.enable = system == "x86_64-linux";
}
