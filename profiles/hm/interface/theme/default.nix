{
  pkgs,
  lib,
  osConfig,
  ...
}: {
  # Set the background
  stylix.image = osConfig.stylix.image;

  # Cursor theme
  stylix.cursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Amber";
    size = 24;
  };
  # TODO until this is fixed in stylix
  home.pointerCursor.name = lib.mkForce "left_ptr";
  home.pointerCursor.package = pkgs.bibata-cursors;
  home.pointerCursor.x11.defaultCursor = lib.mkForce "left_ptr";
}
