{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
  cols = config.theme.colors;
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
}
