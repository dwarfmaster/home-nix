{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = [
    pkgs.gitAndTools.git-annex
  ];
}
