{ home
, lib
, nixos
, master
, unstable
, pkgs
, self
, system
, hosts
, ...
}:

let
  inherit (lib) nameValuePair utils;
  inherit (utils) mapFilterAttrs;
  inherit (builtins) readDir;
  makeTest = pkgs.nixosTest;
  importTest = name: path: makeTest (import path ({ inherit lib pkgs system; } // hosts));
in
mapFilterAttrs
  (_: v: v != null)
  (n: v: if v == "directory"
         then nameValuePair n (importTest n "${./.}/${n}")
         else nameValuePair "" null)
  (readDir ./.)
