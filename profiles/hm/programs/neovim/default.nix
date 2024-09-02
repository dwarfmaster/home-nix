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

  stylix.targets.nixvim.enable = true;
  programs.nixvim = {
    enable = true;
    wrapRc = false;
  };
}
