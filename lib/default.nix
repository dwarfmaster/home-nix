{ lib, pkgs, ... }:

let
  utils = import ./utils.nix { inherit lib pkgs; };
  inherit (utils) recImport;
in recImport { dir = ./.; _import = base: import "${./.}/${base}.nix" { inherit lib pkgs; }; }
