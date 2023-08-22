{
  config,
  pkgs,
  ...
}: {
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
  };
}
