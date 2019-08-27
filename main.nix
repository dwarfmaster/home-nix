{ pkgs, ... }:

self:

let lib     = import ./lib.nix;                                in
let recdata = lib.mergeMod (lib.mayAccess [ "modules" ] self); in
let args    = { inherit pkgs self recdata; };                  in

{
  home.packages = import ./packages.nix args;
  nixpkgs.config = { allowUnfree = true; }; # For unrar

  programs = {
    home-manager = import programs/home-manager args;
    git          = import programs/git          args;
    firefox      = import programs/firefox      args;
  } // lib.mayAccess [ "programs" ] recdata;

  modules = {
    xinit  = import modules/xinit  args;
    xmonad = import modules/xmonad args;
    zsh    = import modules/zsh    args;
  };

  home.file = lib.mayAccess [ "home" "file" ] recdata;
  xdg = lib.mayAccess [ "xdg" ] recdata // { enable = true; };
}

