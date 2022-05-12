{ config, pkgs, ... }:

let
  cols = config.theme.colors;
  cfg = config.services.dunst;

  pauseDunst = pkgs.writeShellScript "dunst-pause"
    "PATH=${pkgs.dbus}/bin:$PATH ${cfg.package}/bin/dunstctl set-paused true";
  unpauseDunst = pkgs.writeShellScript "dunst-unpause"
    "PATH=${pkgs.dbus}/bin:$PATH ${cfg.package}/bin/dunstctl set-paused false";

  notify-send = "${pkgs.libnotify}/bin/notify-send";
  dunstctl = "${config.services.dunst.package}/bin/dunstctl";
  notifier = pkgs.writeShellScriptBin "notify" ''
    case $1 in
      low)
        ${notify-send} --urgency=low "$2" "$3";;
      normal)
        ${notify-send} --urgency=normal "$2" "$3";;
      high)
        ${notify-send} --urgency=critical "$2" "$3";;
      clear)
        ${dunstctl} close-all;;
      progress)
        ${notify-send} -h "int:value:$3" "$2";;
    esac
  '';

in {
  services.dunst = {
    enable = true;
    enableBase16Theme = true;
    settings = {
      global = {
        monitor            = 0;
        geometry           = "385x5-20+40";
        separator_height   = 3;
        transparency       = 0;
        padding            = 5;
        horizontal_padding = 5;
        frame_width        = 3;
        font               = "FuraCode Nerd Font 12";
        format             = "<b>%s</b>\\n%b";
      };

      urgency_low.timeout      = 10;
      urgency_normal.timeout   = 10;
      urgency_critical.timeout = 15;
    };
  };

  home.packages = [ pkgs.libnotify notifier ]; # For notify-send
  applications.notifier = "${notifier}/bin/notify";

  systemd.user.services = {
    pause-dunst-on-lock = {
      Unit = {
        Description = "Pause dunst notifications when active";
        PartOf = [ "xsession-lock.target" ];
      };
      Install = { WantedBy = [ "xsession-lock.target" ]; };
      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pauseDunst}";
        ExecStop  = "${unpauseDunst}";
      };
    };
  };
}
