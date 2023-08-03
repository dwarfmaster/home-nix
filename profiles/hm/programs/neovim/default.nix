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

  services.korrvigs = {
    constants.nvim = "${config.programs.neovim.package}/bin/nvim";
    extraModules.nvim = ./nvim.pl;
  };
}
