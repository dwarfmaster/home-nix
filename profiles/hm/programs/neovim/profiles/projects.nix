{ config, lib, pkgs, ... }:

{
  programs.nixvim = {
    plugins.project-nvim = {
      enable = true;
      silent = false;
    };
    plugins.which-key.bindings.n."<leader>".subs."f".bindings."p" =
      { cmd = "Telescope projects"; description = "Select project"; };
  };
}
