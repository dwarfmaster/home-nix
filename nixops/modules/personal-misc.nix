# Generic configuration shared by all personal computers

{ config, pkgs, ... }:

{
  # Nix configuration
  nix = {
    binaryCaches = [ "https://cache.nixos.org" "https://hydra.iohk.io" "https://cache.dhall-lang.org" ];
    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM="
    ];
    nixPath = [ "nixpkgs=http://nixos.org/channels/nixos-19.03/nixexprs.tar.xz" ];
    useSandbox = true;
  };
  nixpkgs.config.allowUnfree = false;
  system.stateVersion = "19.09";

  # Basic networking configuration
  networking = {
    wireless.enable = false;  # Enables wireless support via wpa_supplicant.
    useDHCP = false;
    networkmanager.enable = true;
    extraHosts = ''
      127.0.0.1 9gag.com www.9gag.com
      127.0.0.1 armorgames.com www.armorgames.com
    '';
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "fr";
    defaultLocale = "en_US.UTF-8";
  };
  time.timeZone = "Europe/Paris";

  # Misc services to enable
  services.ntp.enable = true;
  sound.enable = true;
  sound.enableOSSEmulation = true;
  hardware.pulseaudio.enable = true;
  services.printing.enable = true; # CUPS
  services.avahi.enable = true; # Network printing discovery
  services.samba.enable = true; # Idem
  programs.zsh.enable = true;
  programs.adb.enable = true;
  virtualisation.docker.enable = true;

  # Necessary to have zsh completion for system packages
  environment.pathsToLink = [ "/share/zsh" ];
}

