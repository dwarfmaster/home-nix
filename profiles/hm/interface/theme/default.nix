{
  pkgs,
  lib,
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
  # TODO until this is fixed in stylix
  home.pointerCursor.x11.defaultCursor = lib.mkForce "left_ptr";
}
