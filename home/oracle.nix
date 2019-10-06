{ pkgs, ... }:

self:

let lib     = import ../lib/lib.nix;                                in
let recdata = lib.mergeMod (lib.mayAccess [ "modules" ] self); in
let args    = { inherit pkgs self recdata; };                  in

{
  home.packages = import ./oracle-packages.nix args;
  nixpkgs.config = { allowUnfree = true; }; # For unrar

  modules = {
    zsh          = import modules/zsh          args;
    home-manager = import modules/home-manager args;
    git          = import modules/git          args;
    vim          = import modules/vim          args;
    fzf          = import modules/fzf          args;
  };

  home.file = lib.mayAccess [ "home" "file" ] recdata;
  xdg       = lib.mayAccess [ "xdg" ] recdata // { enable = true; };
  programs  = lib.mayAccess [ "programs" ] recdata;
  services  = lib.mayAccess [ "services" ] recdata;
  systemd   = lib.mayAccess [ "systemd" ] recdata;
}

