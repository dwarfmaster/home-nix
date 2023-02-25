{
  config,
  lib,
  pkgs,
  ...
}: let
  # Taken from
  #   https://gitlab.com/rycee/nur-expressions/-/blob/master/hm-modules/theme-base16/screen-locker.nix
  args = let
    colors = config.lib.stylix.colors.withHashtag;
    b = colors.base00;
    c = colors.base05 + "22";
    d = colors.base0A;
    t = colors.base05;
    w = colors.base08;
    v = colors.base0E;
  in [
    "--color=${b}"
    "--insidever-color=${c}"
    "--ringver-color=${v}"
    "--insidewrong-color=${c}"
    "--ringwrong-color=${w}"
    "--inside-color=${b}"
    "--ring-color=${d}"
    "--line-color=${b}"
    "--separator-color=${d}"
    "--verif-color=${t}"
    "--wrong-color=${t}"
    "--time-color=${t}"
    "--date-color=${t}"
    "--layout-color=${t}"
    "--keyhl-color=${w}"
    "--bshl-color=${w}"
    "--clock"
    "--indicator"
    "--time-str='%H:%M:%S'"
    "--date-str='%a %b %e'"
    "--keylayout 1"
  ];

  i3lock = "${pkgs.i3lock-color}/bin/i3lock-color ${toString args}";
  setxkbmap = "${pkgs.xorg.setxkbmap}/bin/setxkbmap";
  bctl = "${pkgs.brightnessctl}/bin/brightnessctl";

  # TODO Make it a program somewhere else
  unsetxkbmap =
    pkgs.writeScript "unsetxkbmap"
    ''
      #!${config.programs.nushell.package}/bin/nu

      let toremove = $nu.env.XKBOPTION
      # Get all options
      let options = (${setxkbmap} -query | lines | split column ': ' | str trim | pivot -ri | get options | split row ',')
      # Unset all options
      ${setxkbmap} -option
      # Reset all other options
      echo $options | where $it !~ $toremove | each { ${setxkbmap} -option $it }
    '';

  dim-screen =
    pkgs.writeShellScriptBin "dim_screen"
    ''
      BRIGHTNESS=$(${bctl} --class=backlight get)
      trap "${bctl} --class=backlight set $BRIGHTNESS; exit 1" SIGINT SIGTERM
      ${bctl} --class=backlight set 5%
      while true; do
        sleep 1
      done
    '';

  locker =
    pkgs.writeShellScriptBin "lock-xsession"
    ''
      # Activate systemctl target
      systemctl --user start xsession-lock.target
      # Disable switching to VTs
      ${setxkbmap} -option srvrkeys:none
      # Lock X11 screen
      ${i3lock} -n
      # Re-enable switching to VTs
      XKBOPTION=srvrkeys:none ${unsetxkbmap}
      # De-activate systemctl target
      systemctl --user stop xsession-lock.target
    '';
in {
  home.packages = [locker dim-screen pkgs.vlock pkgs.brightnessctl];
  applications.locker = "${pkgs.systemd}/bin/loginctl lock-session";

  xdg.dataFile."applications/screen-locker.desktop".text = ''
    [Desktop Entry]
    Name=Screen Locker
    Exec=${config.applications.locker}
    Type=Application
    Terminal=false
    Categories=System;
    Icon=lock
  '';

  systemd.user.services = {
    dpms-setup = {
      Unit = {
        Description = "Set up DPMS configuration for X11 session";
        After = ["graphical-session-pre.target"];
        PartOf = ["graphical-session.target"];
      };

      Install = {WantedBy = ["graphical-session.target"];};

      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.xorg.xset}/bin/xset s 300 60";
      };
    };
    xsession-locker-setup = {
      Unit = {
        Description = "Screen locker";
        After = ["graphical-session-pre.target" "dpms-setup.service"];
        PartOf = ["graphical-session.target"];
      };

      Install = {WantedBy = ["graphical-session.target"];};

      Service = {
        Type = "simple";
        ExecStart = "${pkgs.xss-lock}/bin/xss-lock -s $XDG_SESSION_ID -n ${dim-screen}/bin/dim_screen -- ${locker}/bin/lock-xsession";
      };
    };
  };
  systemd.user.targets = {
    xsession-lock = {
      Unit = {
        Description = "Target active when the X11 session is locked";
      };
    };
  };
}
