{ config, lib, ... }:

# TODO adapt to base16 theme
let

  inherit (config.pkgsets) pkgs;

  xmonad = pkgs.xmonad-with-packages.override {
    packages = hpkgs: builtins.attrValues {
      inherit (hpkgs) xmonad-contrib xmonad-extras xmobar;
    };
  };

  xmobarrc = pkgs.writeText "xmobarrc" (builtins.readFile ./xmobarrc);

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
    ;
    xmobarCmd = "${pkgs.haskellPackages.xmobar}/bin/xmobar ${xmobarrc}";
  };

in {
  xsession.windowManager.command = "${xmonad-compiled}";
}
