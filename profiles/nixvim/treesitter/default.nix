{
  config,
  pkgs,
  lib,
  ...
}: {
  plugins.treesitter = {
    enable = true;
    nixGrammars = true;
    ensureInstalled = "all";
    incrementalSelection.enable = true;
    indent = true;
    grammarPackages = pkgs.tree-sitter.allGrammars;
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
