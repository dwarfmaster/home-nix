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
      action = lib.nixvim.mkRaw "function() require('telescope').extensions.projects.projects{} end";
      options.desc = "Select project";
    }
  ];
}
