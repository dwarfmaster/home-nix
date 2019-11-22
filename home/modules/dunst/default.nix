general:

let
  pkgs = general.pkgs.main;
in {
  services.dunst = {
    enable = true;
    settings = {
      global = {
        monitor            = 0;
        geometry           = "385x5-20+40";
        separator_height   = 3;
        transparency       = 0;
        padding            = 5;
        horizontal_padding = 5;
        frame_width        = 3;
        frame_color        = "#6eb958";
        separator_color    = "frame";
        font               = "FuraCode Nerd Font 12";
        format             = "<b>%s</b>\\n%b";
      };

      urgency_low = {
        background  = "#48413a";
        foreground  = "#d7c8bc";
        frame_color = "#bb90e2";
        timeout     = 10;
      };

      urgency_normal = {
        background  = "#48413a";
        foreground  = "#d7c8bc";
        frame_color = "#6eb958";
        timeout     = 10;
      };

      urgency_critical = {
        background  = "#48413a";
        foreground  = "#d7c8bc";
        frame_color = "#d35c5c";
        timeout     = 15;
      };
    };
  };

  packages = with pkgs; [ libnotify ]; # For notify-send
}

