{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    python = pkgs.python3.withPackages (ppkgs: [ ]);
  };
}
