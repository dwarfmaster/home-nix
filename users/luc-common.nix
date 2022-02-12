{ lib, config, pkgs, ... }:

{
    imports = [
      # System
      ../config/system/xdg
      ../config/system/direnv
      ../config/system/encryption

      # Interface
      ../config/interface/theme

      # Programs
      ../config/programs/fzf
      ../config/programs/git
      ../config/programs/vim
    ];

    xdg.enable = true;
    programs.git.userName = "DwarfMaster";
    programs.git.userEmail = "luc@dwarfmaster.net";

    manual = {
      html.enable = true;
      json.enable = true;
      manpages.enable = true;
    };
}
