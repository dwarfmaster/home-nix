{ home
, lib
, nixos
, master
, unstable
, pkgset
, self
, system
, utils
, hosts
, ...
}:

let
  inherit (utils) mapFilterAttrs;
  inherit (lib) nameValuePair;
  inherit (builtins) readDir;
  makeTest = pkgset.pkgs.nixosTest;
  importTest = name: path: makeTest (import path ({ inherit lib pkgset system utils; } // hosts));
in
mapFilterAttrs
  (_: v: v != null)
  (n: v: if v == "directory"
         then nameValuePair n (importTest n "${./.}/${n}")
         else nameValuePair "" null)
  (readDir ./.)
