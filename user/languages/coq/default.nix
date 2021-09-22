{ config, lib, ... }:

{
  home.packages = [ config.pkgsets.pkgs.coq ];
}
