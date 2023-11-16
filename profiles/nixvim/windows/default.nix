{
  config,
  lib,
  pkgs,
  ...
}: {
  keymaps = let
    cmd = key: cmd: desc: {
      inherit key;
      action = "<cmd>${cmd}<cr>";
      options.desc = desc;
    };
  in [
    (cmd "<leader>ww" "wincmd p" "Jump to recent")
    (cmd "<leader>wv" "vsplit" "Split vertically")
    (cmd "<leader>w-" "split" "Split horizontally")
    {
      key = "<leader>wD";
      action = "<c-w>ge";
      options.desc = "Detach current window";
    }
    (cmd "<leader>wc" "close" "Close current window")
    (cmd "<leader>wj" "wincmd j" "Jump down")
    (cmd "<leader>wk" "wincmd k" "Jump up")
    (cmd "<leader>wh" "wincmd h" "Jump left")
    (cmd "<leader>wl" "wincmd l" "Jump right")
    (cmd "<leader>wJ" "resize -5" "Decrease height")
    (cmd "<leader>wK" "resize +5" "Increase height")
    (cmd "<leader>wH" "vertical resize -5" "Decrease width")
    (cmd "<leader>wL" "vertical resize +5" "Increase width")
  ];
}
