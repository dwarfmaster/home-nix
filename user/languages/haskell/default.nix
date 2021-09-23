{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      cabal-install
      cabal2nix
      ;
    ghc = pkgs.haskellPackages.ghcWithHoogle
      (hpkgs: [ hpkgs.diagrams ]);
  };
}
