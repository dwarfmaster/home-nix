{ pkgs, ... }:

{
  programs.taskwarrior = {
    enable = true;
    colorTheme = ./color.theme;
    config = {
      default.command = "ready";
    };
  };
}

