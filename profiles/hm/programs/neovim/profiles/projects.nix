{ config, lib, pkgs, ... }:

{
  programs.nixvim = {
    plugins.project-nvim = {
      enable = true;
      silentChdir = false;
    };
    plugins.telescope.extensions.project-nvim.enable = true;
    plugins.which-key.bindings.n."<leader>".subs."f".bindings."p" =
      { lua = "require('telescope').extensions.projects.projects{}"; description = "Select project"; };
  };
}
