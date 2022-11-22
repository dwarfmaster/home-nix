{
  config,
  pkgs,
  ...
}: let
  background-picture = pkgs.copyPathToStore ./bg.png;
  scheme = config.colorScheme;
  fromHex = hex: {
    hex = {
      r = builtins.substring 0 2 hex;
      g = builtins.substring 2 2 hex;
      b = builtins.substring 4 2 hex;
    };
  };
in {
  # Woodland theme, from https://github.com/jcornwall/base16-woodland-scheme
  colorScheme = pkgs.nix-colors.woodland;
  theme.base16 = {
    name = scheme.name;
    kind = "dark";
    colors = {
      base00 = fromHex scheme.colors.base00;
      base01 = fromHex scheme.colors.base01;
      base02 = fromHex scheme.colors.base02;
      base03 = fromHex scheme.colors.base03;
      base04 = fromHex scheme.colors.base04;
      base05 = fromHex scheme.colors.base05;
      base06 = fromHex scheme.colors.base06;
      base07 = fromHex scheme.colors.base07;
      base08 = fromHex scheme.colors.base08;
      base09 = fromHex scheme.colors.base09;
      base0A = fromHex scheme.colors.base01;
      base0B = fromHex scheme.colors.base0A;
      base0C = fromHex scheme.colors.base0C;
      base0D = fromHex scheme.colors.base0D;
      base0E = fromHex scheme.colors.base0E;
      base0F = fromHex scheme.colors.base0F;
    };
  };

  # Setup doom-emacs theme
  programs.doom-emacs.config = {
    initModules = {
      ui = [
        {
          mod = "ligatures";
          args = ["extra"];
        }
        "nav-flash"
        "neotree"
        "ophints"
        {
          mod = "popup";
          args = ["all" "defaults"];
        }
        "window-select"
        "workspace"
      ];
      tools = ["rgb"];
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
        After = ["graphical-session-pre.target"];
        PartOf = ["graphical-session.target"];
      };

      Install = {WantedBy = ["graphical-session.target"];};

      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.imlibsetroot}/bin/imlibsetroot ${background-picture}";
      };
    };
  };
}
