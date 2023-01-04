{
  config,
  pkgs,
  lib,
  ...
}: let
  terminal = let
    conf = pkgs.substituteAll {
      src = ./st.h;
      inherit (pkgs) bash;
      inherit
        (config.colorScheme.colors)
        base00
        base01
        base02
        base03
        base04
        base05
        base06
        base07
        base08
        base09
        base0A
        base0B
        base0C
        base0D
        base0E
        base0F
        ;
    };
    st = pkgs.st.override {conf = builtins.readFile conf;};
  in {
    applications.terminal = "${st}/bin/st";
    home.packages = [st];
    home.sessionVariables.ST_H = "${conf}";
  };

  keyboard = let
    xmodmap = "${pkgs.xorg.xmodmap}/bin/xmodmap";
    remappings = pkgs.writeShellScript "xkb-remap-keys" ''
      ${xmodmap} -e "keycode 65 = space space space space" # shift+space -> 4*space
    '';
  in {
    imports = [ ./sxhkd.nix ];
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
      dconf = pkgs.gnome3.dconf-editor; # GTK configuration editor
    };
  };
in {
  imports = [../graphic-theme xinit keyboard ./dunst.nix ./rofi.nix terminal fonts tools];
}
