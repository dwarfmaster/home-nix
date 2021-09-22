{ lib, config, utils, ... }:

let

  inherit (lib) types;
  inherit (config.pkgsets) pkgs;
  inherit (utils) mkPackagesModule;

  packages = mkPackagesModule config.pkgsets;

in {
  users.users.luc = {
    uid = 1000;
    hashedPassword = import ./password.nix;
    description = "default";
    isNormalUser = true;
    extraGroups = [ "wheel" "luc" "networkmanager" "adbusers" "video" ];
    shell = "/run/current-system/sw/bin/zsh";
  };

  home-manager.users.luc = {
    imports = [
      packages
      ../../../user/core

      # System
      ../../../user/system/xdg
      ../../../user/system/direnv
      ../../../user/system/encryption

      # Programs
      ../../../user/programs/fzf
      ../../../user/programs/git
      ../../../user/programs/vim
    ];

    xdg.enable = true;
    programs.git.userName = "DwarfMaster";
    programs.git.userEmail = "luc@dwarfmaster.net";

    # TODO factor common packages somewhere
    home.packages = with pkgs; [ tree ];

    manual = {
      html.enable = true;
      json.enable = true;
      manpages.enable = true;
    };
  };
}
