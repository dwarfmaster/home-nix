{ pkgs, ... }:

self:

let lib     = import ./lib.nix;                                in
let recdata = lib.mergeMod (lib.mayAccess [ "modules" ] self); in
let args    = { inherit pkgs self recdata; };                  in

{
  home.packages = import ./packages.nix args;

  programs = {
    home-manager = import programs/home-manager args;
    zsh          = import programs/zsh          args;
    git          = import programs/git          args;
    firefox      = import programs/firefox      args;
  };

  modules = {
    xinit  = import modules/xinit  args;
    xmonad = import modules/xmonad args;
  };

  home.file = lib.mayAccess [ "home" "file" ] recdata;
  inherit recdata;
}

