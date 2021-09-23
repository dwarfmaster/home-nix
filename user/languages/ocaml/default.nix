{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      ocaml
      opam
      dune_2
      ;
    utop = pkgs.ocamlPackages.utop;
  };
}
