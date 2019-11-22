general@{ recdata, ... }:

let
  pkgs  = general.pkgs.main; 
  mpkgs = import ./packages.nix general;
in {
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

  packages = with pkgs; with mpkgs; [
    rofi-calc    # Necessary for the XMonad shortcut, but not elegant
    libqalculate # For rofi-calc
  ];

  shellAliases = {
    rofi-calc = "rofi -plugin-path '${mpkgs.rofi-calc}/share/rofi/plugins/' -show calc -modi calc -no-show-match -no-sort -calc-command \"echo '{result}' | xclip\"";
  };
}

