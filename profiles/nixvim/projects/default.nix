{
  config,
  lib,
  pkgs,
  ...
}: {
  plugins.project-nvim = {
    enable = true;
    silentChdir = false;
  };
  plugins.telescope.extensions.project-nvim.enable = true;
  maps.normal."<leader>fp" = {
    action = "function() require('telescope').extensions.projects.projects{} end";
    lua = true;
    desc = "Select project";
  };
}
