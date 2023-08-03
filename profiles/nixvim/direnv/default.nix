{
  config,
  pkgs,
  lib,
  ...
}: {
  extraPlugins = [pkgs.vimPlugins.direnv-vim];
  globals = {
    direnv_auto = 1;
    direnv_edit_mode = "edit"; # one of edit, split, vsplit
    direnv_silent_load = 1;
  };
  maps.normal = {
    "<leader>tD" = {
      action = "<cmd>DirenvExport<cr>";
      desc = "Reload direnv";
    };
    "<leader>oD" = {
      action = "<cmd>EditEnvrc<cr>";
      desc = "Open loaded envrc";
    };
  };
}
