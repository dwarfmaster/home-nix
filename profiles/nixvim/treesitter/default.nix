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
  plugins.which-key.bindings = {
    n."g".subs."n".bindings."n" = {description = "Select treesitter node";};
    n."<leader>".subs."c".bindings."T" = {
      lua = "require'telescope.builtin'.treesitter{}";
      description = "Treesitter symbols";
    };
    v."g".subs."r".bindings = {
      "n" = {description = "Increment treesitter node";};
      "c" = {description = "Increment treesitter scope";};
      "m" = {description = "Decrement treesitter node";};
    };
  };
}
