{ config, lib, ... }:

# I will setup one file per XDG specification, and try to implement them. I
# consider some specifications don't apply to me, so I won't implement those.
# The specification list can be found there :
# https://specifications.freedesktop.org/

let
  inherit (config.pkgsets) pkgs;
in {
  imports = [
    ./autostart.nix
    ./basedir.nix
    ./desktop-entry.nix
    ./mime-apps.nix
  ];

  home.packages = [
    pkgs.gtk4.dev  # Some utilities, most importantly gtk4-icon-browser
  ];
}
