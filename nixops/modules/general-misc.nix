# Generic configuration shared by all computers

{ config, pkgs, ... }:

{
  # Users are now tied to NixOS configuration
  users.mutableUsers = false;
}

