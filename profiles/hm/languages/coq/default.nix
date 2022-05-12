{ config, lib, pkgs, ... }:

let
  packages = pkgs.coqPackages_8_14;
in {
  programs.coq = {
    enable = true;
    inherit packages;
    libraries = [
      pkgs.coq-hott_8_14
    ];
  };
  programs.doom-emacs.config.initModules.lang = [ "coq" ];

  # When installing coq through opam, it needs gmp and zlib
  home.packages = [ pkgs.gmp pkgs.zlib ];
  pkgconfig.enable = true;
  pkgconfig.path = [ "${pkgs.gmp.dev}/lib/pkgconfig" "${pkgs.zlib.dev}/lib/pkgconfig" ];
}
