{ stdenv, lib, writeTextFile,
  fetchurl, fetchgit, fetchhg, fetchsvn,
  php, phpPackages, unzip,
  noDev ? true }:

let
  composerEnv = import ./composer-env.nix {
    inherit stdenv lib writeTextFile fetchurl php unzip phpPackages;
  };
in
import ./php-packages.nix {
  inherit composerEnv noDev;
  inherit fetchurl fetchgit fetchhg fetchsvn;
}
