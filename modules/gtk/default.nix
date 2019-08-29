{ pkgs, ... }:

with pkgs; {
  gtk = {
    enable = true;
    font = {
      package = null; # NERD font already installed
      name    = "FuraCode Nerd Font Bold 14";
    };
    iconTheme = {
      package = papirus-icon-theme;
      name    = "Papirus";
    };
    theme = {
      package = equilux-theme;
      name    = "Equilux";
    };
  };

  packages = [ gnome3.dconf ];
}

