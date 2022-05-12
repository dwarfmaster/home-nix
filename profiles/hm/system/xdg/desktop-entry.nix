{ config, lib, pkgs, ... }:

# SPEC: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html
# Nice interface to create desktop files already present in upstream home-manager
let
  # TODO broken
  lsdesktopfPkg = { stdenv, fetchFromGitHub }:
    stdenv.mkDerivation {
      pname = "lsdesktopf";
      version = "v3f1c437d77b234967e572bb6eb16cc6d0ca02fe3";

      src = fetchFromGitHub {
        owner  = "AndyCrowd";
        repo   = "list-desktop-files";
        rev    = "3f1c437d77b234967e572bb6eb16cc6d0ca02fe3";
        sha256 = "0iawkwp5nlvyvr4g0sn1h7n924kivg9q5v3h7f7ngdcn861hx7bi";
      };

      installPhase = ''
        mkdir -p $out/bin
        install -m 555 lsdesktopf $out/bin
      '';
    };
  lsdesktopf = pkgs.callPackage lsdesktopfPkg { };
in {
  home.packages = [
    pkgs.desktop-file-utils # Tools to work with desktop files
    lsdesktopf              # Script to list desktop files
  ];
}
