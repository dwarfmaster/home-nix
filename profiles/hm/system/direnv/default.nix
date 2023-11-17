{pkgs, ...}: {
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.direnv-vim];
    globals = {
      direnv_auto = 1;
      direnv_edit_mode = "edit"; # one of edit, split, vsplit
      direnv_silent_load = 1;
    };
    keymaps = [
      {
        key = "<leader>tD";
        action = "<cmd>DirenvExport<cr>";
        options.desc = "Reload direnv";
      }
      {
        key = "<leader>oD";
        action = "<cmd>EditEnvrc<cr>";
        options.desc = "Open loaded envrc";
      }
    ];
  };
}
