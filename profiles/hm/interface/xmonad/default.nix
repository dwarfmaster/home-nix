{
  config,
  lib,
  pkgs,
  ...
}: let
  colors = config.lib.stylix.colors;

  xmonad-compiled =
    pkgs.writers.writeHaskell
    "xmonad"
    {
      ghc = pkgs.ghc;
      ghcArgs = ["-i" "${xmonad-nix}"];
      libraries = builtins.attrValues {
        inherit
          (pkgs.haskellPackages)
          xmonad-contrib
          xmonad-extras
          xmobar
          ;
      };
    }
    (builtins.readFile ./xmonad.hs);

  escapeHaskell = lib.escape ["\""];

  makeNixHs = attrs:
    pkgs.writeText "Nix.hs"
    (lib.concatStrings
      (["module Nix where\n\n"]
        ++ (lib.mapAttrsToList (name: value: "${name} :: String\n${name} = \"${escapeHaskell value}\"\n") attrs)));

  xmonad-nix = makeNixHs {
    inherit
      (config.applications)
      calculator
      launcher
      locker
      terminal
      browser
      volume
      brightness
      ;
    focusedColor = "#${colors.base0F}";
    normalColor = "#${colors.base02}";
    rofi = "${pkgs.rofi}/bin/rofi";
    eww = "${config.programs.eww.package}/bin/eww";
  };
in {
  xsession.windowManager.command = "${xmonad-compiled}";
}
