general:

let
  pkgs = general.pkgs.main;
in {
  packages = with pkgs; [ conky ];
  xdg.configFile."conky/conkyrc".source   = ./conkyrc;
  xdg.configFile."conky/conky.lua".source = ./conky.lua;
}

