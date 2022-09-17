{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    plugins.gitgutter = {
      enable = true;
      showMessageOnHunkJumping = false;
      defaultMaps = false;
      previewWinFloating = true;
    };
    plugins.which-key.bindings = {
      n = {
        "]".bindings."c" = { binding = "<Plug>(GitGutterNextHunk)"; description = "Next git hunk"; };
        "[".bindings."c" = { binding = "<Plug>(GitGutterPrevHunk)"; description = "Prev git hunk"; };
        "<leader>".subs."t".bindings = {
          "G" = { cmd = "GitGutterToggle"; description = "Toggle gitgutter"; };
          "g" = { cmd = "GitGutterBufferToggle"; description = "Toggle gitgutter on buffer"; };
        };
        "<leader>".subs."v".bindings = {
          "s" = { cmd = "GitGutterStageHunk"; description = "Stage hunk"; };
          "u" = { cmd = "GitGutterUndoHunk"; description = "Undo staging"; };
          "p" = { cmd = "GitGutterPreviewHunk"; description = "Preview hunk"; };
          "D" = { cmd = "GitGutterDiffOrig"; description = "View diff"; };
          "F" = { cmd = "GitGutterFold"; description = "Fold unchanged"; };
        };
      };
      v."<leader>".subs."v".bindings = {
        "s" = { cmd = "GitGutterStageHunk"; description = "Stage hunk"; };
      };
    };
  };
}
