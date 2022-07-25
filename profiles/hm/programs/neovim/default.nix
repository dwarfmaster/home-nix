{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.unstable.neovide
    pkgs.neovim
    pkgs.fzy
  ];

  programs.nixvim = {
    enable = false;
    extraPlugins = [];
    extraConfigLua = ''
    '';
    options = {
    };

    # Colorscheme
    # TODO switch to base16
    colorschemes.nord = {
      enable = true;
      contrast = true;
      italic = false;
    };

    plugins = {
      # Utils
      comment-nvim.enable = true;
      easyescape.enable = true;
      floaterm.enable = true;
      intellitab.enable = true;
      notify = {
        enable = true;
        stages = "fade_in_slide_out";
      };
      specs.enable = true;
      nvim-tree.enable = true;
      nvim-tree.hijackNetrw = true;
      nvim-autopairs.enable = true;
      startify.enable = true; # Replace with dashboard ?
      surround.enable = true;
      undotree.enable = true;

      # Completion
      coq-nvim.enable = true;
      coq-nvim.installArtifacts = true;
      coq-nvim.autoStart = true;
      coq-nvim.recommendedKeymaps = true;

      # Git
      neogit.enable = true;

      # Languages
      treesitter = {
        enable = true;
        nixGrammars = true;
        ensureInstalled = "all";
      };
      nix.enable = true;

      # LSP
      lsp = {
        enable = true;
        servers.rnix-lsp.enable = true;
        enabledServers = [
          { name = "ccls"; extraOptions = {}; }
          { name = "sumneko_lua"; extraOptions = {}; }
        ];
      };

      # Status line
      lualine = {
        enable = true;
        sections = {};
      };

      # Telescope 
      telescope = {
        enable = true;
        extensions = {
          frecency = {
            enable = true;
            defaultWorkspace = "CWD";
            showUnindexed = true;
          };
          fzy-native.enable = true;
          fzy-native.overrideGenericSorter = true;
        };
      };

    };
  };
}
