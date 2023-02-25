{
  config,
  nixosConfig,
  lib,
  pkgs,
  ...
}: let
  xmobarrc = config.lib.mustache.render "xmobarrc" ./xmobarrc (config.lib.stylix.colors
    // {
      cores =
        lib.concatMapStringsSep "-" (id: "<core${toString id}>")
        (lib.range 0 (nixosConfig.hardware.specs.cores - 1));
      cpus =
        lib.concatMapStringsSep "-" (id: "<total${toString id}>")
        (lib.range 0 (nixosConfig.hardware.specs.threads - 1));
      notify-send = "${pkgs.libnotify}/bin/notify-send";
    });

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
    xmobarCmd = "${pkgs.haskellPackages.xmobar}/bin/xmobar ${xmobarrc}";
  };
in {
  xsession.windowManager.command = "${xmonad-compiled}";
  home.file."xmobarrc".source = "${xmobarrc}";
}
