{
  config,
  lib,
  pkgs,
  ...
}: {
  plugins.which-key.bindings.n."<leader>".subs."w".bindings = {
    "w" = {
      cmd = "wincmd p";
      description = "Jump to recent";
    };
    "v" = {
      cmd = "vsplit";
      description = "Split vertically";
    };
    "-" = {
      cmd = "split";
      description = "Split horizontally";
    };
    "D" = {
      binding = "<c-w>ge";
      description = "Detach current window";
    };
    "c" = {
      cmd = "close";
      description = "Close current window";
    };
    "j" = {
      cmd = "wincmd j";
      description = "Jump down";
    };
    "k" = {
      cmd = "wincmd k";
      description = "Jump up";
    };
    "h" = {
      cmd = "wincmd h";
      description = "Jump left";
    };
    "l" = {
      cmd = "wincmd l";
      description = "Jump right";
    };
    "J" = {
      cmd = "resize -5";
      description = "Decrease height";
    };
    "K" = {
      cmd = "resize +5";
      description = "Increase height";
    };
    "H" = {
      cmd = "vertical resize -5";
      description = "Decrease width";
    };
    "L" = {
      cmd = "vertical resize +5";
      description = "Increase width";
    };
  };
}
