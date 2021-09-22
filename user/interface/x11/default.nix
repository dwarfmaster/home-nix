{ pkgs, config, ... }:

let

  st = pkgs.st.override { conf = builtins.readFile ./st.h; };

  terminal = {
    home.packages = [ st ];
  };

  xmonad = {
    home = {
      file.".xmonad".source = ./xmonad;
      file.".xmonad".recursive = true;
      packages = [
        pkgs.haskellPackages.xmobar
        (pkgs.xmonad-with-packages.override {
          packages = hpkgs: with hpkgs; [ xmonad-contrib xmonad-extras xmobar ];
        })
      ];
    };
  };

  xinit = {
    home.file.".xinitrc".source = ./xinitrc;
    xdg.configFile."bg/bg.png".source = ./bg.png;
  };

  notifications = {
    services.dunst = import ./dunst.nix;
    home.packages = [ pkgs.libnotify ]; # For notify-send
  };

  launcher = {
    programs.rofi = import ./rofi.nix { term = "${st}/bin/st"; inherit config; };
    xdg.configFile."rofi/theme.rasi".source = ./rofi/theme.rasi;
    home.packages = [ pkgs.rofi-calc ];
  };

in {
  imports = [ ../graphic-theme xmonad xinit notifications launcher terminal ];
}

