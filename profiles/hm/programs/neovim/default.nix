{ config, pkgs, lib, ... }:

{
  imports = [
    # Profiles
    ./profiles/core.nix

    # Plugins
    ./plugins/which-keys.nix
  ];

  home.packages = [
    pkgs.unstable.neovide
    # pkgs.neovim
    pkgs.fzy
  ];

  programs.nixvim.enable = true;
}
