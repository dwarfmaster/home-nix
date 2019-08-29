{ pkgs, ... }:

self:

let lib     = import ./lib.nix;                                in
let recdata = lib.mergeMod (lib.mayAccess [ "modules" ] self); in
let args    = { inherit pkgs self recdata; };                  in

{
  home.packages = import ./packages.nix args;
  nixpkgs.config = { allowUnfree = true; }; # For unrar

  modules = {
    xinit        = import modules/xinit        args;
    xmonad       = import modules/xmonad       args;
    zsh          = import modules/zsh          args;
    home-manager = import modules/home-manager args;
    git          = import modules/git          args;
    firefox      = import modules/firefox      args;
    st           = import modules/st           args;
    vim          = import modules/vim          args;
    fzf          = import modules/fzf          args;
    rofi         = import modules/rofi         args;
    clipboard    = import modules/clipboard    args;
    gtk          = import modules/gtk          args;
  };

  home.file = lib.mayAccess [ "home" "file" ] recdata;
  xdg       = lib.mayAccess [ "xdg" ] recdata // { enable = true; };
  programs  = lib.mayAccess [ "programs" ] recdata;
  systemd   = lib.mayAccess [ "systemd" ] recdata;
  qt        = import modules/qt args;
  gtk       = lib.mayAccess [ "gtk" ] recdata;
}

