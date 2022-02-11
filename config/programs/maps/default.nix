{ config, pkgs, ... }:

{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      viking # GPS traces editor
      ;
  };
}
