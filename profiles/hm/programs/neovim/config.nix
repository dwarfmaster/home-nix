{
  config,
  pkgs,
  lib,
  ...
}: let
  tab = 2;
in {
  programs.nixvim = {
    profiles.align.enable = true;
    profiles.telescope.enable = true;
    profiles.treesitter.enable = true;
    profiles.windows.enable = true;
    profiles.projects.enable = true;
    profiles.comments.enable = true;
    profiles.escape.enable = true;
    profiles.notify.enable = true;
    profiles.gitgutter.enable = true;
    profiles.lsp.enable = true;
    profiles.cmp.enable = true;

    #  __  __ _
    # |  \/  (_)___ __
    # | |\/| | (_-</ _|
    # |_|  |_|_/__/\__|
    #
    # Misc
    extraPlugins = [
      pkgs.vimPlugins.vim-repeat
      pkgs.vimPlugins.mini-nvim
    ];
    extraConfigLua = ''
    '';
    # TODO basic settings
    opts = {
      # Number
      number = true;
      relativenumber = true;
      # Search
      smartcase = true;
      ignorecase = true;
      # Indentation
      autoindent = true;
      smartindent = true;
      tabstop = tab;
      shiftwidth = tab;
      expandtab = true;
      # Misc
      mouse = "a";
    };
    globals = {
      mapleader = " ";
      updatetime = 200;
      timeoutlen = 400;
      background = "dark";
    };
    plugins.surround.enable = true;

    #  _____ _
    # |_   _| |_  ___ _ __  ___
    #   | | | ' \/ -_) '  \/ -_)
    #   |_| |_||_\___|_|_|_\___|
    #
    # Theme
    # Theme is set by stylix
    plugins.lualine.enable = true;

    # __      ___    _    _       _
    # \ \    / / |_ (_)__| |_ ___| |_____ _  _
    #  \ \/\/ /| ' \| / _| ' \___| / / -_) || |
    #   \_/\_/ |_||_|_\__|_||_|  |_\_\___|\_, |
    #                                     |__/
    # Which-key
    plugins.which-key = {
      enable = true;
      settings = {
        win = {
          padding = [ 1 2 ];
          border = "none";
          wo.winblend = 10;
        };
      };
    };
    keymaps = let
      mk = key: desc: {
        inherit key;
        options.desc = desc;
      };
    in [
      # (mk "<leader>f" "file")
      # (mk "<leader>c" "code")
      # (mk "<leader>j" "jump")
      # (mk "<leader>b" "buffer")
      # (mk "<leader>o" "open/close")
      # (mk "<leader>v" "git")
      # (mk "<leader>s" "search")
      # (mk "<leader>d" "debug")
      # (mk "<leader>w" "window")
      # (mk "<leader>h" "help")
      # (mk "<leader>t" "toggle")
      # (mk "<leader> " "local")
    ];
    # maps.normal = {
    # };
  };
}
