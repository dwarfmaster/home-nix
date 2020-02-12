{ pkgs, recdata, ... }:

let

  myplugins = import ./plugins.nix { inherit pkgs; };

  lib = import ../../../lib/lib.nix;

  mypkgs = import ./packages.nix { inherit pkgs; };

in {
  programs.neovim = {
    enable      = true;
    withNodeJs  = true;
    withPython  = true;
    withPython3 = true;
    withRuby    = true;
    viAlias     = true;
    vimAlias    = true;

    configure = {
      customRC = builtins.readFile ./vimrc;

      plug.plugins = with pkgs.vimPlugins; with myplugins; [
        # Apparence
        base16-vim-recent    # Base16 color schemes (fixed for neovim nightly)
        lightline-vim        # Status line
        base16-vim-lightline # Base16 color schemes for lightline

        # QOL
        vim-multiple-cursors # Brings multiple cursors to vim
        fzfWrapper
        fzf-vim              # Fuzzy finder for new files
        vim-gitgutter        # Display git information in the gutter
        sandwich             # Operations on sandwiched expressions (parens, brackets ...)
        vim-easy-align       # Alignement operations

        # Intellisense
        coc        # Generic intellisense engine
        vim-pandoc # Support for pandoc

        # Languages
        vim-polyglot      # Support for 144 languages
        vim-pandoc-syntax # Syntax for pandoc
      ];
    };
  };

  packages = with pkgs; [
    nodejs # For coc
  ];

  xdg.configFile."nvim/coc-settings.json".text =
    let cocConfig = import ./coc-config.nix { inherit pkgs; }; in lib.toJSON cocConfig;
  xdg.configFile."nvim/parfait-datalog.vim".source = ./parfait-datalog.vim;
}

