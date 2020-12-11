general@{ lib, recdata, ... }:

let
  myplugins = import ./plugins.nix general;
  pkgs = general.pkgs.main;
in {
  programs.vim = {
    enable      = true;
    extraConfig = builtins.readFile ./vimrc;
    plugins = with pkgs.vimPlugins; with myplugins; [
      # Apparence
      base16-vim-recent    # Base16 color schemes (fixed for neovim nightly)
      lightline-vim        # Status line
      base16-vim-lightline # Base16 color schemes for lightline

      # QOL
      fzfWrapper
      fzf-vim              # Fuzzy finder for new files
      vim-gitgutter        # Display git information in the gutter

      # Languages
      vim-polyglot      # Support for 144 languages
      dhall-vim         # Dhall support
      vim-pandoc-syntax # Pandoc syntax
      coquille          # COQ support
    ];
  };
}
