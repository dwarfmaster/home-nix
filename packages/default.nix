{ pkgs, ... }:

{
  reupload = pkgs.callPackage ./reupload { };
  coq-hott = pkgs.coqPackages.callPackage ./coq-hott { };
}
