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
    ];
    extraConfigLua = ''
    '';
    # TODO basic settings
    options = {
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
    plugins.lualine = {
      enable = true;
      theme = "base16";
    };
    colorschemes.base16 = let
      colors = config.lib.stylix.colors;
    in {
      enable = true;
      colorscheme = colors.scheme-slug;
      customColorScheme = {
        base00 = "#${colors.base00}";
        base01 = "#${colors.base01}";
        base02 = "#${colors.base02}";
        base03 = "#${colors.base03}";
        base04 = "#${colors.base04}";
        base05 = "#${colors.base05}";
        base06 = "#${colors.base06}";
        base07 = "#${colors.base07}";
        base08 = "#${colors.base08}";
        base09 = "#${colors.base09}";
        base0A = "#${colors.base0A}";
        base0B = "#${colors.base0B}";
        base0C = "#${colors.base0C}";
        base0D = "#${colors.base0D}";
        base0E = "#${colors.base0E}";
        base0F = "#${colors.base0F}";
      };
    };

    # __      ___    _    _       _
    # \ \    / / |_ (_)__| |_ ___| |_____ _  _
    #  \ \/\/ /| ' \| / _| ' \___| / / -_) || |
    #   \_/\_/ |_||_|_\__|_||_|  |_\_\___|\_, |
    #                                     |__/
    # Which-key
    plugins.which-key = {
      enable = true;
      window = {
        border = "none";
        margin.left = 2;
        margin.right = 2;
        margin.bottom = 0;
        margin.top = 1;
        winblend = 10;
      };
      keyLabels = {
        " " = "<space>";
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
