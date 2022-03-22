{ config, lib, pkgs, ... }:

{
  programs.coq = {
    enable = true;
    libraries = [
      (pkgs.coq-hott.override { version = "8.13.1"; })
    ];
  };
  programs.doom-emacs.config.initModules.lang = [ "coq" ];

  # When installing coq through opam, it needs gmp
  home.packages = [ pkgs.gmp ];
  pkgconfig.enable = true;
  pkgconfig.path = [ "${pkgs.gmp.dev}/lib/pkgconfig" ];
}
