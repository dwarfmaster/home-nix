{ config, lib, pkgs, ... }:

let
  packages = pkgs.unstable.coqPackages_8_15;
in {
  programs.coq = {
    enable = true;
    inherit packages;
    libraries = [
      (packages.callPackage ../../../packages/coq-hott { })
    ];
  };
  programs.doom-emacs.config.initModules.lang = [ "coq" ];

  # When installing coq through opam, it needs gmp
  home.packages = [ pkgs.gmp ];
  pkgconfig.enable = true;
  pkgconfig.path = [ "${pkgs.gmp.dev}/lib/pkgconfig" ];
}
