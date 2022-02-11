{ config, pkgs, ... }:

{
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
}
