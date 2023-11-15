{
  pkgs,
  ...
}: let
  vim-shakespeare = pkgs.vimUtils.buildVimPlugin {
    name = "vim-syntax-shakespeare";
    src = pkgs.fetchFromGitHub {
      owner = "pbrisbin";
      repo = "vim-syntax-shakespeare";
      rev = "2f4f61eae55b8f1319ce3a086baf9b5ab57743f3";
      sha256 = "0h79c3shzf08g7mckc7438vhfmxvzz2amzias92g5yn1xcj9gl5i";
    };
  };
in {
  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      cabal-install
      cabal2nix
      ;
    ghc =
      pkgs.haskellPackages.ghcWithHoogle
      (hpkgs: builtins.attrValues {
        inherit (hpkgs)
        diagrams
        xmonad
        xmonad-utils
        xmonad-extras
        xmonad-contrib
        ;
      });
  };

  programs.doom-emacs.config = {
    initModules = {
      lang = [
        {
          mod = "haskell";
          args = ["dante"];
        }
      ];
    };
  };

  programs.nixvim = {
    plugins.lsp.servers.hls.enable = true;
    extraPlugins = [ vim-shakespeare ];
    globals = {
      hamlet_prevent_invalid_nesting = 0;
      hamlet_highlight_trailing_space = 0;
    };
  };
}
