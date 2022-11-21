{ lib, ... }:

{
  users.users.luc = {
    uid = 1000;
    hashedPassword = import ./password.nix;
    description = "default";
    isNormalUser = true;
    extraGroups = [ "wheel" "luc" "networkmanager" "adbusers" "video" ];
    shell = "/run/current-system/sw/bin/zsh";
  };

  home-manager.users.luc = {
    imports = builtins.attrValues {
      # System
      inherit (lib.hm.system)
        xdg
        direnv
        encryption
      ;

      # Interface
      inherit (lib.hm.interface)
        theme
      ;

      # Programs
      inherit (lib.hm.programs)
        fzf
        git
        vim
        neovim
      ;
    };

    xdg.enable = true;
    programs.git.userName = "DwarfMaster";
    programs.git.userEmail = "luc@dwarfmaster.net";

    manual = {
      html.enable = true;
      json.enable = true;
      manpages.enable = true;
    };
  };
}
