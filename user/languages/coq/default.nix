{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      coq;
  };
  programs.doom.initModules.lang = [ "coq" ];
  # I need to tell opam where to find gmp
  pkgconfig.enable = true;
  pkgconfig.path = [ "${pkgs.gmp.dev}/lib/pkgconfig" ];
}
