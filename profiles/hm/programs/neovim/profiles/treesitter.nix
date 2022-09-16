{ config, pkgs, lib, ...}:

{
  programs.nixvim = {
    plugins.treesitter = {
      enable = true;
      nixGrammars = false; # TODO
      ensureInstalled = "all";
      incrementalSelection.enable = true;
      indent = true;
    };
    plugins.which-key.bindings = {
      n."g".subs."n".bindings."n" = { description = "Select treesitter node"; };
      v."g".subs."r".bindings = {
        "n" = { description = "Increment treesitter node"; };
	"c" = { description = "Increment treesitter scope"; };
	"m" = { description = "Decrement treesitter node"; };
      };
    };
  };
}
