{ config, lib, ... }:

{
  home.packages = [ config.pkgsets.pkgs.coq ];
  programs.doom.initModules.lang = [ "coq" ];
}
