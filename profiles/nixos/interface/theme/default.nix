{ config, pkgs, ... }:

let
  nerdfonts = pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; };
  font = {
    package = nerdfonts;
    name = "FiraCode Nerd Font Mono";
  };
in {
  stylix = {
    autoEnable = false;
    home-manager = {
      enable = true;
      useSystemTheme = true;
    };
    fonts = {
      serif = font;
      sansSerif = font;
      monospace = font;
      emoji = font;
    };
    image = ./bg.png;
  };
}
