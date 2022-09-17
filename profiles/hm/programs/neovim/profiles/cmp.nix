{ config, pkgs, lsp, ... }:

{
  # TODO more complete setup
  programs.nixvim = {
    plugins.nvim-cmp = {
      enable = true;
      mappingPresets = [ "insert" "cmdline" ];
      sources = [
        { name = "nvim_lsp"; }
        { name = "buffer"; }
        { name = "path"; }
      ];
    };
  };
}
