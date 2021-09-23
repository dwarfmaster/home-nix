{ config, lib, pkgs, ... }:

{
  imports = [
    ./zsh.nix
    ./bash.nix
  ];
}
