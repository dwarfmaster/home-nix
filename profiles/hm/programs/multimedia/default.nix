{
  pkgs,
  ...
}: let
  mvi = pkgs.mpvScripts.mpv-image-viewer;
in {
  home.packages = [
    # Downloader
    pkgs.yt-dlp # Video downloader
  ];

  programs.mpv = {
    enable = true;
    scripts = [
      mvi.image-positioning
    ];
    config = {
      image-display-duration = "inf";
      loop-file = "inf";
      window-dragging = "no";
      background = "color";
      background-color = "0.2";
    };
    bindings = {
      "n" = "repeatable playlist-next";
      "p" = "repeatable playlist-prev";

      # Transformations
      "+" = "add video-zoom 0.5";
      "-" = "add video-zoom -0.5; script-message reset-pan-if-visible";
      "ctrl+WHEEL_UP" = "script-message cursor-centric-zoom 0.1";
      "ctrl+WHEEL_DOWN" = "script-message cursor-centric-zoom -0.1";
      "=" = "no-osd set video-zoom 0; script-message reset-pan-if-visible";
      ")" = "script-message rotate-video 90; show-text \"Clockwise rotation\"";
      "(" = "script-message rotate-video -90; show-text \"Direct rotation\"";

      # Panning
      "WHEEL_UP" = "repeatable script-message pan-image y -0.02 yes yes";
      "WHEEL_DOWN" = "repeatable script-message pan-image y +0.02 yes yes";
      "WHEEL_LEFT" = "repeatable script-message pan-image x -0.02 yes yes";
      "WHEEL_RIGHT" = "repeatable script-message pan-image x +0.02 yes yes";
      "MBTN_LEFT" = "script-binding drag-to-pan";
      "ctrl+down" = "repeatable script-message pan-image y -0.1 yes yes";
      "ctrl+up" = "repeatable script-message pan-image y +0.1 yes yes";
      "ctrl+right" = "repeatable script-message pan-image x -0.1 yes yes";
      "ctrl+left" = "repeatable script-message pan-image x +0.1 yes yes";
      "c" = "no-osd set video-pan-x 0; no-osd set video-pan-y 0";
    };
  };
}
