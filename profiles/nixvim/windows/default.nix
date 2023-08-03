{
  config,
  lib,
  pkgs,
  ...
}: {
  maps.normal = let
    cmd = cmd: desc: {
      action = "<cmd>${cmd}<cr>";
      inherit desc;
    };
  in {
    "<leader>ww" = cmd "wincmd p" "Jump to recent";
    "<leader>wv" = cmd "vsplit" "Split vertically";
    "<leader>w-" = cmd "split" "Split horizontally";
    "<leader>wD" = {
      action = "<c-w>ge";
      desc = "Detach current window";
    };
    "<leader>wc" = cmd "close" "Close current window";
    "<leader>wj" = cmd "wincmd j" "Jump down";
    "<leader>wk" = cmd "wincmd k" "Jump up";
    "<leader>wh" = cmd "wincmd h" "Jump left";
    "<leader>wl" = cmd "wincmd l" "Jump right";
    "<leader>wJ" = cmd "resize -5" "Decrease height";
    "<leader>wK" = cmd "resize +5" "Increase height";
    "<leader>wH" = cmd "vertical resize -5" "Decrease width";
    "<leader>wL" = cmd "vertical resize +5" "Increase width";
  };
}
