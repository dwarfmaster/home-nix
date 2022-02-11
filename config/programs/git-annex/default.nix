{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.gitAndTools.git-annex
  ];
}
