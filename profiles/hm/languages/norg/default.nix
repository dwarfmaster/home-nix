{
  config,
  pkgs,
  ...
}: let
  plugins = pkgs.vimPlugins;
  neorg = pkgs.vimUtils.buildVimPlugin {
    pname = "neorg";
    version = "2c18f831effb5f3f383b2049902be23ea0dd7a8e";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-neorg";
      repo = "neorg";
      rev = "2c18f831effb5f3f383b2049902be23ea0dd7a8e";
      sha256 = "10lwjrwxjmag2jbhpzfv9g6zpl7mlmv646gr062pragb2cphrm7n";
    };
    dependencies = [plugins.plenary-nvim (plugins.nvim-treesitter.withPlugins (_: []))];
  };
  tree-sitter-norg = pkgs.tree-sitter-make-grammar {
    language = "norg";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-neorg";
      repo = "tree-sitter-norg";
      rev = "600874e0825a62c3822ff04e5d0efbac8d7e380c";
      sha256 = "1l200zd7xfhifaxq3i810j2bxpvm0yfgf6x2i2d83l34z32q3p4x";
    };
    inherit (pkgs.tree-sitter) version;
  };
  tree-sitter-norg-meta = pkgs.tree-sitter-make-grammar {
    language = "norg-meta";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-neorg";
      repo = "tree-sitter-norg-meta";
      rev = "4696d8ec5a67926085e3740a69b3e925419798fc";
      sha256 = "0siwgj0yxz956r1gz0h277nj7aqspy7kb9b04xd3mzc1v90r60yn";
    };
    inherit (pkgs.tree-sitter) version;
  };
in {
  programs.nixvim = {
    # plugins.treesitter.grammarPackages = [
    #   tree-sitter-norg
    #   tree-sitter-norg-meta
    # ];
    extraPlugins = [
      # neorg
      /*
      neorg-telescope
      */
    ];
    # extraConfigLua = ''
    #   require('neorg').setup {
    #     load = {
    #       ["core.defaults"] = {},
    #       ["core.norg.completion"] = {
    #         config = { engine = "nvim-cmp" }
    #       },
    #     }
    #   }
    # '';
  };
}
