{
  config,
  lib,
  pkgs,
  ...
}: {
  plugins.project-nvim = {
    enable = true;
    enableTelescope = true;
    settings.silent_chdir = false;
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
