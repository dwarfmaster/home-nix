{
  config,
  pkgs,
  ...
}: let
  copier = pkgs.writeScript "xcopier" ''
    echo "$1" | ${pkgs.xclip}/bin/xclip -i
  '';
in {
  home.packages =
    [
      pkgs.libsForQt5.okular # Heavyweight pdf reader
    ]
    ++ builtins.attrValues {
      inherit
        (pkgs)
        # Viewers
        
        abiword # Graphical text editor
        libreoffice
        # Conversion
        
        qpdf # Content preserving pdf transformations
        pandoc # Markdown converter
        
        # Downloaders
        
        fanficfare # Download and convert to epub fanfiction from the web
        ;
    };
  programs.zathura = {
    enable = true;
    mappings = {
      "u" = "exec ${pkgs.reupload}/bin/reupload\ \"$FILE\"";
      "C" = "exec ${copier}\ \"$FILE\"";
    };
    options = {
      database = "null";
      # For syntex
      dbus-service = true;
      dbus-raise-window = true;
      synctex = true;
    };
  };
  stylix.targets.zathura.enable = true;
}
