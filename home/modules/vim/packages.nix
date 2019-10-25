{ pkgs, ... }:

{
  # Made it works thanks to https://github.com/gloaming/nixpkgs/tree/feature/nightly-neovim
  neovim-nightly = 
    let
      pkgs = import <nixos-unstable> {};
    in pkgs.neovim;
}

