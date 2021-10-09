{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  programs.rofi = {
    enable   = true;
    cycle    = true;
    terminal = "${pkgs.st}/bin/st";
    package  = pkgs.rofi.override {
      plugins = builtins.attrValues {
        inherit (pkgs) rofi-emoji rofi-calc;
      };
    };

    enableBase16Theme = true;
    borderWidth = 2;
    font        = "FuraCode Nerd Font Bold 20";
    fullscreen  = false;
    lines       = 15;
    location    = "center";
    scrollbar   = true;
    separator   = "solid";
    padding     = 5;

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
  applications.calculator = "${config.programs.rofi.package}/bin/rofi -modi calc -show calc";
}
