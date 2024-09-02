{
  pkgs,
  lib,
  config,
  ...
}:
{
  imports = [../../system/xdg];

  qt = {
    enable = true;
    platformTheme.name = "gtk";
    style.name = "gtk2";
  };

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.numix-icon-theme;
      name = "Numix";
    };
  };

  fonts.fontconfig.enable = true;

  # Tell firefox and XDG-compliant utilities to use my directories
  xdg.configFile."user-dirs.dirs".source = ./dirs;

  # Try fixing the gtk file chooser dialog window being bigger than screen
  # TODO doesn't work
  xdg.configFile."gtk-2.0/gtkFileChooser.ini".source = ./gtkFileChooser.ini;
  dconf.enable = true;
  dconf.settings."org/gtk/settings/file-chooser" = {
    window-position = "(480, 270)";
    window-size = "(960, 540)";
  };
}
