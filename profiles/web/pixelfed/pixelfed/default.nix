{ storage ? null, envConfig, stdenv, lib, writeTextFile,
  fetchurl, fetchgit, fetchhg, fetchsvn,
  php, phpPackages, unzip, writeText,
  noDev ? true }:

let
  pixelfed-app = writeText "app.php"
    (import ./app.nix { inherit storage lib; });
  pixelfed-db = writeText "database.php"
    (import ./db.nix { inherit lib; });
  pixelfed-env = writeText "env"
    (import ./pixelfed-env.nix { config = envConfig; inherit lib; });
  composerEnv = import ./composer-env.nix {
    inherit stdenv lib writeTextFile fetchurl php unzip phpPackages;
  };
in
import ./php-packages.nix {
  inherit pixelfed-app pixelfed-db pixelfed-env;
  inherit composerEnv noDev;
  inherit fetchurl fetchgit fetchhg fetchsvn;
}
