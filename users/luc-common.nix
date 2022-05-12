{ lib, config, pkgs, ... }:

{
    imports = builtins.attrValues {
      # System
      inherit (lib.profiles.system)
        xdg
        direnv
        encryption
      ;

      # Interface
      inherit (lib.profiles.interface)
        theme
      ;

      # Programs
      inherit (lib.profiles.programs)
        fzf
        git
        vim
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
}
