{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    # Profiles
    ./config.nix
  ];

  home.packages = [
    pkgs.unstable.neovide
    # pkgs.neovim
    pkgs.fzy
  ];

  programs.nixvim.enable = true;
  # programs.neovim.package = pkgs.neovim-nightly;
}
