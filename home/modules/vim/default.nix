general@{ lib, recdata, ... }:

let

  myplugins = import ./plugins.nix general;

  mypkgs = import ./packages.nix general;

  unstable = general.pkgs.nixpkgs.nixos-unstable;

  pkgs = general.pkgs.main;

in {
  programs.neovim = {
    enable      = true;
    package     = mypkgs.neovim-nightly;
    withNodeJs  = true;
    withPython  = true;
    withPython3 = true;
    withRuby    = true;
    viAlias     = true;
    vimAlias    = true;

    extraPython3Packages = ppkgs: [ (mypkgs.python-tasklib ppkgs) ];

    extraConfig = builtins.readFile ./vimrc;
    plugins = with pkgs.vimPlugins; with myplugins; [
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

      # Intellisense
      coc # Generic intellisense engine

      # Languages
      vim-polyglot      # Support for 144 languages
      dhall-vim         # Dhall support
      vim-pandoc-syntax # Pandoc syntax
      vim-pandoc        # Advanced pandoc support
      vim-pandoc-after  # Compatibility layer between vim-pandoc and other plugins

      # Org-mode lite
      vimwiki
      taskwiki
    ];
  };

  packages = with pkgs; [
    nodejs # For coc
  ];

  xdg.configFile."nvim/coc-settings.json".text =
    let cocConfig = import ./coc-config.nix general; in lib.toJSON cocConfig;
}

