{
  pkgs,
  lib,
  config,
  ...
}: let
  # TODO upstream to stylix
  rycee-nur = pkgs.fetchFromSourcehut {
    owner = "~rycee";
    repo = "nur-expressions";
    rev = "db2f2ff538c8c755e6b062c9be1c514752c6ee1a";
    sha256 = "0g2hzw9lp5dyvnn9gq786jlar0jn5mdc9ya6brs87is3lnlqc1fy";
  };
  colors = config.lib.stylix.colors;
  configBase16 = {
    name = colors.scheme-name;
    kind = config.stylix.polarity;
    colors = {
      base00.hex.rgb = colors.base00;
      base01.hex.rgb = colors.base01;
      base02.hex.rgb = colors.base02;
      base03.hex.rgb = colors.base03;
      base04.hex.rgb = colors.base04;
      base05.hex.rgb = colors.base05;
      base06.hex.rgb = colors.base06;
      base07.hex.rgb = colors.base07;
      base08.hex.rgb = colors.base08;
      base09.hex.rgb = colors.base09;
      base0A.hex.rgb = colors.base0A;
      base0B.hex.rgb = colors.base0B;
      base0C.hex.rgb = colors.base0C;
      base0D.hex.rgb = colors.base0D;
      base0E.hex.rgb = colors.base0E;
      base0F.hex.rgb = colors.base0F;
    };
  };
  theme = (import rycee-nur {inherit pkgs;}).materia-theme.override {
    inherit configBase16;
  };
in {
  imports = [../../system/xdg];

  qt = {
    enable = true;
    platformTheme = "gtk";
  };

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.numix-icon-theme;
      name = "Numix";
    };
    theme = lib.mkForce {
      name = config.lib.stylix.colors.scheme-name;
      package = theme;
    };
    font = lib.mkForce {
      name = "${config.stylix.fonts.sansSerif.name} 12";
      package = config.stylix.fonts.sansSerif.package;
    };
  };
  # It's not doing anything as of now
  stylix.targets.gtk.enable = true;

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
