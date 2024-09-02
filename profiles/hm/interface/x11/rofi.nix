{
  config,
  pkgs,
  ...
}: {
  programs.rofi = {
    enable = true;
    cycle = true;
    terminal = "${pkgs.st}/bin/st";
    plugins = builtins.attrValues {
      inherit (pkgs) rofi-emoji rofi-calc;
    };

    location = "center";

    configPath = "${config.xdg.configHome}/rofi/config.rasi";
    extraConfig = {
      matching = "fuzzy";
    };
  };

  applications.launcher = "${config.programs.rofi.package}/bin/rofi -modi drun -show drun -show-icons";
  applications.calculator = "${config.programs.rofi.package}/bin/rofi -modi calc -show calc -no-show-match -no-sort -calc-command \"echo '{result}' | ${pkgs.xclip}/bin/xclip -i\"";
}
