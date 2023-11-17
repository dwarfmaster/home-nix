{
  config,
  pkgs,
  ...
}: {
  # Set the background using feh
  stylix.targets.feh.enable = true;

  # Cursor theme
  stylix.cursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Amber";
    size = 24;
  };
}
