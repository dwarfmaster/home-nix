{ config, pkgs, lib, ... }:

{
  imports = [
    # Profiles
    ./profiles/core.nix

    # HM Plugins
    ./plugins/nix-colors.nix
  ];

  home.packages = [
    pkgs.unstable.neovide
    # pkgs.neovim
    pkgs.fzy
  ];

  # The functional syntax is necessary because of https://github.com/NixOS/nixpkgs/issues/70638
  programs.nixvim = {...}: {
    enable = true;
    imports = [
      # Nixvim plugins
      ./plugins/which-keys.nix
    ];
  };
  # programs.neovim.package = pkgs.neovim-nightly;
}
