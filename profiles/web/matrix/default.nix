{ config, lib, pkgs, ... }:

{
  imports = [
    ./synapse.nix
    ./irc.nix
  ];
}
