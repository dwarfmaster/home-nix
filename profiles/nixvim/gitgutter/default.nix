{
  config,
  pkgs,
  lib,
  ...
}: {
  plugins.gitgutter = {
    enable = true;
    showMessageOnHunkJumping = false;
    defaultMaps = false;
    previewWinFloating = true;
  };
  maps.normal = let
    picker = name: desc: {
      action = "function() require'telescope.builtin'.${name}{} end";
      lua = true;
      inherit desc;
    };
  in {
    "]c" = {
      action = "<Plug>(GitGutterNextHunk)";
      desc = "Next git hunk";
    };
    "[c" = {
      action = "<Plug>(GitGutterPrevHunk)";
      desc = "Prev git hunk";
    };
    "<leader>tG" = {
      action = "<cmd>GitGutterToggle<cr>";
      desc = "Toggle gitgutter";
    };
    "<leader>tg" = {
      action = "<cmd>GitGutterBufferToggle<cr>";
      desc = "Toggle gitgutter on buffer";
    };
    "<leader>vs" = {
      action = "<cmd>GitGutterStageHunk<cr>";
      desc = "Stage hunk";
    };
    "<leader>vu" = {
      action = "<cmd>GitGutterUndoHunk<cr>";
      desc = "Undo staging";
    };
    "<leader>vp" = {
      action = "<cmd>GitGutterPreviewHunk<cr>";
      desc = "Preview hunk";
    };
    "<leader>vD" = {
      action = "<cmd>GitGutterDiffOrig<cr>";
      desc = "View diff";
    };
    "<leader>vF" = {
      action = "<cmd>GitGutterFold<cr>";
      desc = "Fold unchanged";
    };
    "<leader>vc" = picker "git_commits" "List commits";
    "<leader>vC" = picker "git_bcommits" "Diff to commits";
    "<leader>vB" = picker "git_branches" "List branches";
    "<leader>vS" = picker "git_status" "List changes";
    "<leader>vZ" = picker "git_stash" "List stash items";
  };
  maps.visual = {
    "<leader>vs" = {
      action = "<cmd>GitGutterStageHunk<cr>";
      desc = "Stage hunk";
    };
  };
}
