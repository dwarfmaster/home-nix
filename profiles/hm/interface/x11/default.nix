{
  config,
  pkgs,
  lib,
  ...
}: let
  keyboard = let
    xmodmap = "${pkgs.xorg.xmodmap}/bin/xmodmap";
    remappings = pkgs.writeShellScript "xkb-remap-keys" ''
      ${xmodmap} -e "keycode 65 = space space space space" # shift+space -> 4*space
    '';
  in {
    imports = [./sxhkd.nix];
    home.keyboard.layout = "fr";
    systemd.user.services = {
      xkbmappings = {
        Unit = {
          Description = "Common X key remapping";
          After = ["graphical-session-pre.target" "setxkbmap.service"];
          PartOf = ["graphical-session.target"];
        };

        Install = {WantedBy = ["graphical-session.target"];};

        Service = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = "${remappings}";
        };
      };
    };
  };

  xinit = {
    xsession = {
      enable = true;
      scriptPath = ".xinitrc";
      # DBus is not setted up properly with the startx method
      initExtra = ''
        if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
          eval $(dbus-launch --exit-with-session --sh-syntax)
        fi
        if command -v dbus-update-activation-environment >/dev/null 2>&1; then
          dbus-update-activation-environment DISPLAY XAUTHORITY
        fi
      '';
    };
    home.packages =
      [pkgs.xorg.xrandr]
      ++ builtins.attrValues {
        inherit (pkgs) imlibsetroot;
      };
  };

  fonts = {
    home.packages = builtins.attrValues {
      # Fonts with icons
      nerdfonts = pkgs.nerdfonts.override {fonts = ["FiraCode"];};
      inherit
        (pkgs)
        powerline-fonts
        ;

      # Misc fonts
      inherit
        (pkgs)
        iosevka
        fira-code
        fira-mono
        fira-code-symbols
        ;

      # Misc
      inherit
        (pkgs)
        font-manager # Preview fonts
        ;
    };
  };

  tools = {
    home.packages = builtins.attrValues {
      inherit
        (pkgs)
        glxinfo # OpenGL info
        redshift # Color shift with the time of the day
        xclip # X11 copy-paste from the console
        networkmanagerapplet # GUI for network manager
        ;
      inherit
        (pkgs.xorg)
        xev # X11 event querying
        xprop # X11 properties querying
        ;
      dconf = pkgs.dconf-editor; # GTK configuration editor
    };
  };
in {
  imports = [../graphic-theme xinit keyboard ./dunst.nix ./rofi.nix fonts tools];
}
