{ system, config, lib, pkgs, ... }:

{
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
