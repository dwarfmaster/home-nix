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
  keymaps = [
    {
      key = "<leader>fp";
      action = "function() require('telescope').extensions.projects.projects{} end";
      lua = true;
      options.desc = "Select project";
    }
  ];
}
