{ config, lib, pkgs, ... }:

{
  imports = [
    ./synapse.nix
    ./element.nix
    ./irc.nix
  ];
}
