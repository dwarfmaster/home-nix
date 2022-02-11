{ home
, lib
, nixos
, master
, unstable
, pkgs
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
  makeTest = pkgs.nixosTest;
  importTest = name: path: makeTest (import path ({ inherit lib pkgs system utils; } // hosts));
in
mapFilterAttrs
  (_: v: v != null)
  (n: v: if v == "directory"
         then nameValuePair n (importTest n "${./.}/${n}")
         else nameValuePair "" null)
  (readDir ./.)
