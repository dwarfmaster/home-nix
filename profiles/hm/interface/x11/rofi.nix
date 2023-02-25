{
  config,
  pkgs,
  ...
}: let
  themefile = config.lib.stylix.colors {
    template = builtins.readFile ./rofi-theme.mustache;
    extension = "rasi";
  };
in {
  programs.rofi = {
    enable = true;
    cycle = true;
    terminal = "${pkgs.st}/bin/st";
    plugins = builtins.attrValues {
      inherit (pkgs) rofi-emoji rofi-calc;
    };

    font = "${config.stylix.fonts.monospace.name} 20";
    location = "center";
    theme = "stylix";

    configPath = "${config.xdg.configHome}/rofi/config.rasi";
    extraConfig = {
      matching = "fuzzy";
    };
  };
  xdg.configFile."rofi/themes/stylix.rasi".source = themefile;

  applications.launcher = "${config.programs.rofi.package}/bin/rofi -modi drun -show drun -show-icons";
  applications.calculator = "${config.programs.rofi.package}/bin/rofi -modi calc -show calc -no-show-match -no-sort -calc-command \"echo '{result}' | ${pkgs.xclip}/bin/xclip -i\"";
}
