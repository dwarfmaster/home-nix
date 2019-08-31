{ pkgs, ... }:

let
  lib   = import ../../lib.nix;
  mpkgs = import ./packages.nix { inherit pkgs; };
in {
  programs.firefox = {
    enable = true;
    enableIcedTea = true;
  };

  home.file.".mozilla/native-messaging-hosts/rofi_interface.json".text = lib.toJSON {
    name               = "rofi_interface";
    description        = "Native backend for rofi tab switcher.";
    path               = "${mpkgs.rofi-tab-switcher}/bin/rofiface.py";
    type               = "stdio";
    allowed-extensions = [ "@rofi.tab.switcher" ];
  };
}

