general@{ lib, ... }:

let
  mpkgs = import ./packages.nix general;
in {
  programs.firefox = {
    enable = true;
    enableIcedTea = true;
  };
}

