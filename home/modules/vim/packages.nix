general:

let
  pkgs = general.pkgs.main;
in {
  # Made it works thanks to https://github.com/gloaming/nixpkgs/tree/feature/nightly-neovim
  neovim-nightly = 
    let
      pkgs = general.pkgs.nixpkgs.nixos-unstable;
    in pkgs.neovim;
}

