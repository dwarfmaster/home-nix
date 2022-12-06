{pkgs, ...}: {
  imports = [../../system/xdg];

  qt = {
    enable = true;
    platformTheme = "gtk";
  };

  gtk = {
    enable = true;
    font = {
      package = null; # nerdfonts installed globally
      name = "FiraCode Nerd Font 14";
    };
    iconTheme = {
      package = pkgs.numix-icon-theme;
      name = "Numix";
    };
    enableBase16Theme = true;
    #theme = {
    #  package = pkgs.equilux-theme;
    #  name    = "Equilux";
    #};
  };

  # TODO improve font handling
  fonts = {
    fontconfig.enable = true;
  };

  home.packages = [pkgs.dconf];

  # Tell firefox and XDG-compliant utilities to use my directories
  xdg.configFile."user-dirs.dirs".source = ./dirs;

  # Try fixing the gtk file chooser dialog window being bigger than screen
  # TODO doesn't work
  xdg.configFile."gtk-2.0/gtkFileChooser.ini".source = ./gtkFileChooser.ini;
  dconf = {
    enable = true;
    settings = {
      "org/gtk/settings/file-chooser" = {
        window-position = "(480, 270)";
        window-size = "(960, 540)";
      };
    };
  };
}
