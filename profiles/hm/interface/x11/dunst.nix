{
  config,
  pkgs,
  ...
}: let
  cols = config.colorScheme.colors;
  cfg = config.services.dunst;

  pauseDunst =
    pkgs.writeShellScript "dunst-pause"
    "PATH=${pkgs.dbus}/bin:$PATH ${cfg.package}/bin/dunstctl set-paused true";
  unpauseDunst =
    pkgs.writeShellScript "dunst-unpause"
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
    settings = {
      global = {
        monitor = 0;
        follow = "none";
        history_length = 1000;
        notification_limit = 7;
        indicate_hidden = true;

        width = 706;
        height = 100;
        origin = "top-right";
        offset = "5x30";

        separator_height = 2;
        separator_color = "#${cols.base02}";
        frame_width = 2;
        corner_radius = 7;
        transparency = 20;

        padding = 5;
        horizontal_padding = 5;
        font = "FiraCode Nerd Font 12";
        format = "<b>%s</b>\\n%b";
      };

      urgency_low = {
        timeout = 10;
        background = "#${cols.base01}";
        foreground = "#${cols.base05}";
        frame_color = "#${cols.base0B}";
      };
      urgency_normal = {
        timeout = 10;
        background = "#${cols.base01}";
        foreground = "#${cols.base05}";
        frame_color = "#${cols.base0E}";
      };
      urgency_critical = {
        timeout = 15;
        background = "#${cols.base01}";
        foreground = "#${cols.base05}";
        frame_color = "#${cols.base08}";
      };
    };
  };

  home.packages = [pkgs.libnotify notifier]; # For notify-send
  applications.notifier = "${notifier}/bin/notify";

  systemd.user.services = {
    pause-dunst-on-lock = {
      Unit = {
        Description = "Pause dunst notifications when active";
        PartOf = ["xsession-lock.target"];
      };
      Install = {WantedBy = ["xsession-lock.target"];};
      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pauseDunst}";
        ExecStop = "${unpauseDunst}";
      };
    };
  };
}
