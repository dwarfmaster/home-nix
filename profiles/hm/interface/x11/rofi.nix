{
  config,
  pkgs,
  ...
}: let
  cfg = config.programs.rofi;
  rofi = "${cfg.finalPackage}/bin/rofi";
in {
  programs.rofi = {
    enable = true;
    cycle = true;
    terminal = config.applications.terminal;
    plugins = builtins.attrValues {
      inherit (pkgs) rofi-emoji rofi-calc;
    };

    location = "center";

    configPath = "${config.xdg.configHome}/rofi/config.rasi";
    extraConfig = {
      matching = "fuzzy";
    };
  };

  applications.launcher = "${rofi} -modi drun -show drun -show-icons";
  applications.calculator = "${rofi} -modi calc -show calc -no-show-match -no-sort -qalc-binary ${pkgs.libqalculate}/bin/qalc | cut -d= -f2 | tr -d ' ' | ${pkgs.xclip}/bin/xclip -i";
}
