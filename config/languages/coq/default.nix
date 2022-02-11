{ config, lib, pkgs, ... }:

{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      coq;
  };
  programs.doom-emacs.config.initModules.lang = [ "coq" ];
  # I need to tell opam where to find gmp
  pkgconfig.enable = true;
  pkgconfig.path = [ "${pkgs.gmp.dev}/lib/pkgconfig" ];
}
