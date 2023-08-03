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
      (hpkgs: [hpkgs.diagrams]);
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
