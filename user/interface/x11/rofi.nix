{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  programs.rofi = {
    enable   = true;
    cycle    = true;
    terminal = "${pkgs.st}/bin/st";
    plugins  = builtins.attrValues {
      inherit (pkgs) rofi-emoji rofi-calc;
    };

    enableBase16Theme = true;
    font        = "FuraCode Nerd Font Bold 20";
    location    = "center";

    # scrollbar   = true;
    # separator   = "solid";
    # padding     = 5;
    # fullscreen  = false;
    # borderWidth = 2;
    # lines       = 15;

    configPath  = "${config.xdg.configHome}/rofi/config.rasi";
    extraConfig = {
      # Enable the extending coloring options
      color-enabled = true;
      # Sorting method
      sort = "fzf";
      matching = "fuzzy";
      # Two columns
      columns = 2;
    };
  };

  applications.launcher   = "${config.programs.rofi.package}/bin/rofi -modi drun -show drun -show-icons";
  applications.calculator = "${config.programs.rofi.package}/bin/rofi -modi calc -show calc -no-show-match -no-sort -calc-command \"echo '{result}' | ${pkgs.xclip}/bin/xclip -i\"";
}
