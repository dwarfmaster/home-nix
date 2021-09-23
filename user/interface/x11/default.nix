{ config, ... }:

let

  inherit (config.pkgsets) pkgs;

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
    home.packages = [ pkgs.xorg.xrandr ] ++ builtins.attrValues {
      inherit (pkgs) imlibsetroot;
    };
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

  fonts = {
    home.packages = builtins.attrValues {
      # Fonts with icons
      nerdfonts = pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; };
      inherit (pkgs)
        powerline-fonts
        ;

      # Misc fonts
      inherit (pkgs)
        iosevka
        fira-code
        fira-mono
        fira-code-symbols
        ;

      # Misc
      inherit (pkgs)
        font-manager # Preview fonts
        ;
    };
  };

  tools = {
    home.packages = builtins.attrValues {
      inherit (pkgs)
        desktop-file-utils  # Manage desktop files
        glxinfo             # OpenGL info
        i3lock              # Screen locker
        redshift            # Color shift with the time of the day
        xclip               # X11 copy-paste from the console
        ;
      inherit (pkgs.xorg)
        xev                 # X11 event querying
        xprop               # X11 properties querying
        ;
      dconf = pkgs.gnome3.dconf-editor; # GTK configuration editor
    };
  };

in {
  imports = [ ../graphic-theme xmonad xinit notifications launcher terminal fonts tools];
}

