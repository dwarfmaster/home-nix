{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = [ pkgs.lean ];
}
