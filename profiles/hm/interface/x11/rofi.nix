{ config, pkgs, ... }:

let
  theme = pkgs.writeText "hm-base16.rasi"
    (import ./rofi-theme.nix config.theme.base16.colors);
in {
  programs.rofi = {
    enable   = true;
    cycle    = true;
    terminal = "${pkgs.st}/bin/st";
    plugins  = builtins.attrValues {
      inherit (pkgs) rofi-emoji rofi-calc;
    };

    font     = "FuraCode Nerd Font Bold 20";
    location = "center";
    theme    = "hm-base16";

    configPath  = "${config.xdg.configHome}/rofi/config.rasi";
    extraConfig = {
      matching = "fuzzy";
    };
  };
  xdg.configFile."rofi/themes/hm-base16.rasi".source = theme;

  applications.launcher   = "${config.programs.rofi.package}/bin/rofi -modi drun -show drun -show-icons";
  applications.calculator = "${config.programs.rofi.package}/bin/rofi -modi calc -show calc -no-show-match -no-sort -calc-command \"echo '{result}' | ${pkgs.xclip}/bin/xclip -i\"";
}
