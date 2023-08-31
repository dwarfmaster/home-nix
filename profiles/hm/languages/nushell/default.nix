{ pkgs, ... }:

let
  tree-sitter-nu = pkgs.tree-sitter-make-grammar {
    language = "nu";
    src = pkgs.fetchFromGitHub {
      owner = "nushell";
      repo = "tree-sitter-nu";
      rev = "786689b0562b9799ce53e824cb45a1a2a04dc673";
      sha256 = "1v0g3ygr0j9bm9159in9k0q4f19gv93wzhp5r9gi71labp98mp0h";
    };
    inherit (pkgs.tree-sitter) version;
  };
in {
  programs.nushell.enable = true;

  # Integrations
  programs.direnv.enableNushellIntegration = true;

  # Neovim support
  programs.nixvim = {
    plugins.treesitter.grammarPackages = [
      tree-sitter-nu
    ];
    autoCmd = [
      {
        event = [ "BufNewFile" "BufRead" ];
        pattern = [ "*.nu" ];
        command = "set ft=nu";
      }
    ];
  };
}
