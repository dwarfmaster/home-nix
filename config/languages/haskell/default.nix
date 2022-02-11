{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      cabal-install
      cabal2nix
      ;
    inherit (pkgs.haskellPackages)
      structured-haskell-mode
      ;
    ghc = pkgs.haskellPackages.ghcWithHoogle
      (hpkgs: [ hpkgs.diagrams ]);
  };

  programs.doom-emacs.config = {
    initModules = {
      lang = [ { mod = "haskell"; args = [ "dante" ]; } ];
    };
  };
}
