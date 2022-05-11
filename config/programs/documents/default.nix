{ config, pkgs, ... }:

let
  copier = pkgs.writeScript "xcopier" ''
    echo "$1" | ${pkgs.xclip}/bin/xclip -i
  '';
in {
  home.packages = [
    pkgs.libsForQt5.okular # Heavyweight pdf reader
  ] ++ builtins.attrValues {
    inherit (pkgs)
      # Viewers
      zathura     # lightweight pdf reader
      abiword     # Graphical text editor
      libreoffice

      # Conversion
      qpdf        # Content preserving pdf transformations
      pandoc      # Markdown converter

      # Downloaders
      fanficfare  # Download and convert to epub fanfiction from the web
      ;
  };
  xdg.configFile."zathura/zathurarc".text = ''
    map u exec ${pkgs.reupload}/bin/reupload\ "$FILE"
    map C exec ${copier}\ "$FILE"

    set database null
    # For SyncTex
    set dbus-service true
    set dbus-raise-window true
    set synctex true
  '';
}
