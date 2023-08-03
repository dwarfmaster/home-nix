{
  config,
  pkgs,
  lib,
  ...
}: {
  plugins.comment-nvim = {
    enable = true;
    sticky = true; # Avoid moving the cursor
  };
  maps.normal = {
    "gc".desc = "Comments";
    "gcc".desc = "Toggle comment";
    "gco".desc = "Insert comment next line";
    "gcO".desc = "Insert comment previous line";
    "gcA".desc = "Insert comment end of line";
    "gb".desc = "Block comments";
    "gbc".desc = "Toggle block comment";
  };
  maps.visual = {
    "gc".desc = "Toggle comment";
    "gb".desc = "Toggle block comment";
  };
  plugins.which-key.operators = {
    "gc" = "Comments";
    "gb" = "Block comments";
  };
}
