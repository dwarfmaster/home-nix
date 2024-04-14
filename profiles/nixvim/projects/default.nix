{
  config,
  lib,
  pkgs,
  ...
}: {
  plugins.project-nvim = {
    enable = true;
    enableTelescope = true;
    silentChdir = false;
  };
  keymaps = [
    {
      key = "<leader>fp";
      action = "function() require('telescope').extensions.projects.projects{} end";
      lua = true;
      options.desc = "Select project";
    }
  ];
}
