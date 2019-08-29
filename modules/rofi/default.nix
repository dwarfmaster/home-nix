{ pkgs, recdata, ... }:

{
  programs.rofi = {
    enable   = true;
    cycle    = true;
    terminal = recdata.misc.terminal;

    borderWidth = 2;
    theme       = "/home/luc/.config/rofi/theme.rasi"; # TODO BAD !
    font        = "FuraCode Nerd Font Bold 20";
    fullscreen  = false;
    lines       = 15;
    location    = "center";
    scrollbar   = true;
    separator   = "solid";
    padding     = 5;

    extraConfig = builtins.readFile ./config;
  };

  xdg.configFile."rofi/theme.rasi".source = ./theme.rasi;
}

