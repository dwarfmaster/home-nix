{ lib, ... }:

let
  utils = import ./utils.nix { inherit lib; };
  inherit (utils) recImport;
in recImport { dir = ./.; _import = base: import "${./.}/${base}.nix" { inherit lib; }; }
