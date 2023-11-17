{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.plugins.treesitter;
in {
  plugins.treesitter = {
    enable = true;
    nixGrammars = true;
    ensureInstalled = "all";
    incrementalSelection.enable = true;
    indent = true;
    grammarPackages = cfg.package.passthru.allGrammars;
    nixvimInjections = true;
  };
  keymaps = [
    # "gnn".desc = "Select treesitter node";
    {
      key = "<leader>cT";
      action = "function() require'telescope.builtin'.treesitter{} end";
      lua = true;
      options.desc = "Treesitter symbols";
    }
  ];
  # maps.visual = {
  #   "grn".desc = "Increment treesitter node";
  #   "grc".desc = "Increment treesitter scope";
  #   "grm".desc = "Decrement treesitter node";
  # };
}
