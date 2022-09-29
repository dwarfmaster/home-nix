{ config, pkgs, lib, ... }:

{
  imports = [
    # Profiles
    ./profiles/core.nix

    # Plugins
    ./plugins/which-keys.nix
    ./plugins/projects.nix
    ./plugins/nix-colors.nix
    ./plugins/neorg.nix
  ];

  home.packages = [
    pkgs.unstable.neovide
    # pkgs.neovim
    pkgs.fzy
  ];

  programs.nixvim.enable = true;
  # programs.neovim.package = pkgs.neovim-nightly;
}
