{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;

  physlock = "${pkgs.physlock}/bin/physlock";
  i3lock = builtins.replaceStrings ["%%"] ["%"] "${config.services.screen-locker.lockCmd}";
  vlock = "${pkgs.vlock}/bin/vlock";
  setxkbmap = "${pkgs.xorg.setxkbmap}/bin/setxkbmap";
  bctl = "${pkgs.brightnessctl}/bin/brightnessctl";

  # TODO Make it a program somewhere else
  unsetxkbmap = pkgs.writeScript "unsetxkbmap"
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

  dim-screen = pkgs.writeShellScriptBin "dim_screen"
    ''
      BRIGHTNESS=$(${bctl} --class=backlight get)
      trap "${bctl} --class=backlight set $BRIGHTNESS; exit 1" SIGINT SIGTERM
      ${bctl} --class=backlight set 5%
      while true; do
        sleep 1
      done
    '';

  locker = pkgs.writeShellScriptBin "lock-xsession"
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
  services.screen-locker.enableBase16Theme = true;
  home.packages = [ locker dim-screen pkgs.vlock pkgs.brightnessctl ];
  applications.locker = "${pkgs.systemd}/bin/loginctl lock-session";

  systemd.user.services = {
    dpms-setup = {
      Unit = {
        Description = "Set up DPMS configuration for X11 session";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.xorg.xset}/bin/xset s 300 60";
      };
    };
    xsession-locker-setup = {
      Unit = {
        Description = "Screen locker";
        After = [ "graphical-session-pre.target" "dpms-setup.service" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

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
