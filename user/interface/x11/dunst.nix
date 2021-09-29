{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
  cols = config.theme.colors;
  cfg = config.services.dunst;

  pauseDunst = pkgs.writeShellScript "dunst-pause"
    "PATH=${pkgs.dbus}/bin:$PATH ${cfg.package}/bin/dunstctl set-paused true";
  unpauseDunst = pkgs.writeShellScript "dunst-unpause"
    "PATH=${pkgs.dbus}/bin:$PATH ${cfg.package}/bin/dunstctl set-paused false";
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

  home.packages = [ pkgs.libnotify ]; # For notify-send

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
