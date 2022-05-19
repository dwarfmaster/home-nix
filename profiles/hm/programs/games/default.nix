{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.unstable.cataclysm-dda
  ];
}
