{ config, pkgs, ... }:

let
  background-picture = pkgs.copyPathToStore ./bg.png;
in {
  # Woodland theme, from https://github.com/jcornwall/base16-woodland-scheme
  theme.base16 = {
    name = "Woodland";
    kind = "dark";
    colors = pkgs.base16Schemes.woodland.Woodland;
  };

  # Setup doom-emacs theme
  programs.doom-emacs.config = {
    initModules = {
      ui = [
        { mod = "ligatures"; args = [ "extra" ]; }
        "nav-flash"
        "neotree"
        "ophints"
        { mod = "popup"; args = [ "all" "defaults" ]; }
        "window-select"
        "workspace"
      ];
      tools = [ "rgb" ];
    };
    modules.ui.dwarfmaster-theme = {
      config.source = ./config.el;
    };
  };

  # Set the background when running X
  systemd.user.services = {
    xbackground = {
      Unit = {
        Description = "Set the background image";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.imlibsetroot}/bin/imlibsetroot ${background-picture}";
      };
    };
  };
}
