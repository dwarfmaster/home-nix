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
    pkgs.neovide
    pkgs.fzy
  ];

  programs.nixvim = {
    enable = true;
    wrapRc = false;
  };
}
