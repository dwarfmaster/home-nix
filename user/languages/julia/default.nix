{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      # TODO won't evaluate because libgit2 is unsafe
      #julia       # Julia interpreter
      ;
  };
}
