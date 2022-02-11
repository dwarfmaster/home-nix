{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs.idrisPackages; [
    (with-packages [ lightyear contrib ])
  ];
  programs.doom-emacs.config.initModules.lang = [ "idris" ];
}
