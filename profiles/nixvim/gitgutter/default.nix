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
  keymaps = let
    picker = key: name: desc: {
      inherit key;
      action = "function() require'telescope.builtin'.${name}{} end";
      lua = true;
      options.desc = desc;
    };
  in [
    {
      key = "]c";
      action = "<Plug>(GitGutterNextHunk)";
      options.desc = "Next git hunk";
    }
    {
      key = "[c";
      action = "<Plug>(GitGutterPrevHunk)";
      options.desc = "Prev git hunk";
    }
    {
      key = "<leader>tG";
      action = "<cmd>GitGutterToggle<cr>";
      options.desc = "Toggle gitgutter";
    }
    {
      key = "<leader>tg";
      action = "<cmd>GitGutterBufferToggle<cr>";
      options.desc = "Toggle gitgutter on buffer";
    }
    {
      key = "<leader>vs";
      action = "<cmd>GitGutterStageHunk<cr>";
      options.desc = "Stage hunk";
    }
    {
      key = "<leader>vu";
      action = "<cmd>GitGutterUndoHunk<cr>";
      options.desc = "Undo staging";
    }
    {
      key = "<leader>vp";
      action = "<cmd>GitGutterPreviewHunk<cr>";
      options.desc = "Preview hunk";
    }
    {
      key = "<leader>vD";
      action = "<cmd>GitGutterDiffOrig<cr>";
      options.desc = "View diff";
    }
    {
      key = "<leader>vF";
      action = "<cmd>GitGutterFold<cr>";
      options.desc = "Fold unchanged";
    }
    (picker "<leader>vc" "git_commits" "List commits")
    (picker "<leader>vC" "git_bcommits" "Diff to commits")
    (picker "<leader>vB" "git_branches" "List branches")
    (picker "<leader>vS" "git_status" "List changes")
    (picker "<leader>vZ" "git_stash" "List stash items")
  ];
}
