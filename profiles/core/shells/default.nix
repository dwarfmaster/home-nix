{ config, lib, pkgs, ... }:

{
  imports = [
    ./zsh.nix
    ./bash.nix
  ];

  # Disable nix proposing derivations for command not found
  programs.command-not-found.enable = false;
}
