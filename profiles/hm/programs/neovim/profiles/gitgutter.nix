{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.nixvim = {
    plugins.gitgutter = {
      enable = true;
      showMessageOnHunkJumping = false;
      defaultMaps = false;
      previewWinFloating = true;
    };
    plugins.which-key.bindings = let
      picker = name: desc: {
        lua = "require'telescope.builtin'.${name}{}";
        description = desc;
      };
    in {
      n = {
        "]".bindings."c" = {
          binding = "<Plug>(GitGutterNextHunk)";
          description = "Next git hunk";
        };
        "[".bindings."c" = {
          binding = "<Plug>(GitGutterPrevHunk)";
          description = "Prev git hunk";
        };
        "<leader>".subs."t".bindings = {
          "G" = {
            cmd = "GitGutterToggle";
            description = "Toggle gitgutter";
          };
          "g" = {
            cmd = "GitGutterBufferToggle";
            description = "Toggle gitgutter on buffer";
          };
        };
        "<leader>".subs."v".bindings = {
          "s" = {
            cmd = "GitGutterStageHunk";
            description = "Stage hunk";
          };
          "u" = {
            cmd = "GitGutterUndoHunk";
            description = "Undo staging";
          };
          "p" = {
            cmd = "GitGutterPreviewHunk";
            description = "Preview hunk";
          };
          "D" = {
            cmd = "GitGutterDiffOrig";
            description = "View diff";
          };
          "F" = {
            cmd = "GitGutterFold";
            description = "Fold unchanged";
          };
          "c" = picker "git_commits" "List commits";
          "C" = picker "git_bcommits" "Diff to commits";
          "B" = picker "git_branches" "List branches";
          "S" = picker "git_status" "List changes";
          "Z" = picker "git_stash" "List stash items";
        };
      };
      v."<leader>".subs."v".bindings = {
        "s" = {
          cmd = "GitGutterStageHunk";
          description = "Stage hunk";
        };
      };
    };
  };
}
