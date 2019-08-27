self:

{ pkgs, ... }:

let lib = import ./lib.nix; in

{
  home.packages = import ./packages.nix { inherit pkgs self; };

  programs.home-manager = {
    enable = true;
    path = https://github.com/rycee/home-manager/archive/release-19.03.tar.gz;
  };

  programs.zsh     = import programs/zsh     { inherit pkgs self; };
  programs.git     = import programs/git     { inherit pkgs self; };
  programs.firefox = import programs/firefox { inherit pkgs self; };

  home.file.".xinitrc".source = ./xinitrc;
  home.file.".xmonad".source = ./xmonad;
  home.file.".xmonad".recursive = true;

}

