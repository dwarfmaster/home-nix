{ pkgs, ... }:

{
  reupload = pkgs.callPackage ./reupload { };
}
