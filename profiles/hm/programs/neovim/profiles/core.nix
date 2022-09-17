{ config, pkgs, lib, ... }:

let
  tab = 4;
in {
  imports = [
    ./telescope.nix
    ./treesitter.nix
    ./windows.nix
    ./projects.nix
    ./comments.nix
    ./escape.nix
    ./notify.nix
  ];

  programs.nixvim = {
    #  __  __ _        
    # |  \/  (_)___ __ 
    # | |\/| | (_-</ _|
    # |_|  |_|_/__/\__|
    #                  
    # Misc
    extraPlugins = [];
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


    #  _____ _                  
    # |_   _| |_  ___ _ __  ___ 
    #   | | | ' \/ -_) '  \/ -_)
    #   |_| |_||_\___|_|_|_\___|
    #                           
    # Theme
    # TODO enable support for colorscheme not based on name but on colors
    # Maybe user nvim-base16
    colorschemes.base16 = {
      enable = true;
      useTruecolor = true;
      colorscheme = lib.toLower config.colorScheme.name;
    };
    plugins.lualine = {
      enable = true;
      # TODO "base16" here requires nvim-base16
      # Using auto in the meantime
      theme = "auto";
    };


    # __      ___    _    _       _            
    # \ \    / / |_ (_)__| |_ ___| |_____ _  _ 
    #  \ \/\/ /| ' \| / _| ' \___| / / -_) || |
    #   \_/\_/ |_||_|_\__|_||_|  |_\_\___|\_, |
    #                                     |__/ 
    # Which-key
    plugins.which-key = {
      enable = true;
      popup.window = {
        border = "none";
        margin.left = 2;
        margin.right = 2;
        margin.bottom = 0;
        blend = 70;
      };
      labels = {
        " " = "<space>";
      };
      bindings.n = {
        "<leader>".subs = {
          "f".name = "file";
          "c".name = "code";
          "j".name = "jump";
          "b".name = "buffer";
          "o".name = "open/close";
          "v".name = "git";
          "s".name = "search";
          "d".name = "debug";
          "w".name = "window";
          "h".name = "help";
          " ".name = "local";
        };
      };
    };
  };
}
