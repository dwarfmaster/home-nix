{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in {
  reupload = callPackage ./reupload { };
}
