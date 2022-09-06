{ config, lib, pkgs, ... }:

# TODO adapt to base16 theme
let

  xmonad = pkgs.xmonad-with-packages.override {
    packages = hpkgs: builtins.attrValues {
      inherit (hpkgs) xmonad-contrib xmonad-extras xmobar;
    };
  };

  xmobarrc = pkgs.substituteAll {
    src = ./xmobarrc;
    inherit (config.colorScheme.colors)
      base00 base01 base02 base03 base04 base05 base06 base07
      base08 base09 base0A base0B base0C base0D base0E base0F;
  };

  xmonad-compiled =
    pkgs.writers.writeHaskell
      "xmonad"
      { ghc = pkgs.ghc;
        ghcArgs = [ "-i" "${xmonad-nix}" ];
        libraries = builtins.attrValues {
          inherit (pkgs.haskellPackages)
            xmonad-contrib
            xmonad-extras
            xmobar;
        };
      }
      (builtins.readFile ./xmonad.hs);

  escapeHaskell = lib.escape [ "\"" ];

  makeNixHs = attrs: pkgs.writeText "Nix.hs"
    (lib.concatStrings
      ([ "module Nix where\n\n" ]
       ++ (lib.mapAttrsToList (name: value: "${name} :: String\n${name} = \"${escapeHaskell value}\"\n") attrs)));

  xmonad-nix = makeNixHs {
    inherit (config.applications)
      calculator
      launcher
      locker
      terminal
      browser
      volume
      brightness
    ;
    xmobarCmd = "${pkgs.haskellPackages.xmobar}/bin/xmobar ${xmobarrc}";
  };

in {
  xsession.windowManager.command = "${xmonad-compiled}";
}
