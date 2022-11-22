{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.direnv-vim];
    globals = {
      direnv_auto = 1;
      direnv_edit_mode = "edit"; # one of edit, split, vsplit
      direnv_silent_load = 1;
    };
    plugins.which-key.bindings.n."<leader>".subs = {
      "t".bindings."D" = {
        cmd = "DirenvExport";
        description = "Reload direnv";
      };
      "o".bindings."D" = {
        cmd = "EditEnvrc";
        description = "Open loaded envrc";
      };
    };
  };
}
