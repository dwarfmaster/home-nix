{ config, pkgs, lib, ... }:

{
  imports = [ ./telescope.nix ];

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
    options = {
      number = true;
      relativenumber = true;
    };
    globals = {
      mapleader = " ";
    };

    #   ___     _                _                  
    #  / __|___| |___ _ _ ___ __| |_  ___ _ __  ___ 
    # | (__/ _ \ / _ \ '_(_-</ _| ' \/ -_) '  \/ -_)
    #  \___\___/_\___/_| /__/\__|_||_\___|_|_|_\___|
    #                                               
    # Colorscheme
    # TODO enable support for colorscheme not based on name but on colors
    # Maybe user nvim-base16
    colorschemes.base16 = {
      enable = true;
      useTruecolor = true;
      colorscheme = lib.toLower config.colorScheme.name;
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
