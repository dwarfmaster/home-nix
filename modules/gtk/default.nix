{ pkgs, ... }:

with pkgs; {
  gtk = {
    enable = true;
    font = {
      package = null; # NERD font already installed
      name    = "FuraCode Nerd Font 14";
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

  # Necessary to fix gtk file chooser dialog window bigger than screen
  # TODO doesn't work
  xdf.configFile."gtk-2.0/gtkFileChooser.ini".source = gtkFileChooser.ini;

  dconf.enable = true;
  dconf.settings = {
    # Idem ><'
    "org/gtk/settings/file-chooser" = {
      window-position = "(480, 270)";
      window-size     = "(960, 540)";
    };
  };
}

