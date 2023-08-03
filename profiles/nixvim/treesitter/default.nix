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
    grammarPackages =
      builtins.filter
      (grammar: grammar.pname != "tree-sitter-norg-grammar")
      pkgs.tree-sitter.allGrammars;
  };
  maps.normal = {
    "gnn".desc = "Select treesitter node";
    "<leader>cT" = {
      action = "function() require'telescope.builtin'.treesitter{} end";
      lua = true;
      desc = "Treesitter symbols";
    };
  };
  maps.visual = {
    "grn".desc = "Increment treesitter node";
    "grc".desc = "Increment treesitter scope";
    "grm".desc = "Decrement treesitter node";
  };
}
