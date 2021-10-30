{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = with pkgs.idrisPackages; [
    (with-packages [ lightyear contrib ])
  ];
  programs.doom.initModules.lang = [ "idris" ];
}
