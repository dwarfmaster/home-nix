{
  config,
  lib,
  pkgs,
  ...
}:
# SPEC: https://specifications.freedesktop.org/mime-apps-spec/mime-apps-spec-latest.html
# Mostly handled by home-manager
{
  xdg.mime.enable = true;
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      # Documents
      "application/pdf" = ["org.pwmt.zathura.desktop"];
      "application/x-cbt" = ["org.pwmt.zathura.desktop"];
      "application/x-cbr" = ["org.pwmt.zathura.desktop"];
      "application/x-cbz" = ["org.pwmt.zathura.desktop"];
      "application/vnd.comicbook-rar" = ["org.pwmt.zathura.desktop"];
      "application/vnd.comicbook+zip" = ["org.pwmt.zathura.desktop"];

      # Videos
      "video/x-matroska" = ["mpv.desktop"];
      "video/x-msvideo" = ["mpv.desktop"];
      "video/mp4" = ["mpv.desktop"];
      "video/ogg" = ["mpv.desktop"];
      "video/webm" = ["mpv.desktop"];
      "video/3gpp" = ["mpv.desktop"];
      "video/3gpp2" = ["mpv.desktop"];

      # Pictures
      "image/jpeg" = ["mpv.desktop"];
      "image/png" = ["mpv.desktop"];
      "image/gif" = ["mpv.desktop"];
      "image/x-ms-bmp" = ["mpv.desktop"];
      "image/webp" = ["mpv.desktop"];
      "audio/3gpp" = ["mpv.desktop"];
      "audio/3gpp2" = ["mpv.desktop"];

      # Audio
      "audio/aac" = ["mpv.desktop"];
      "audio/mpeg" = ["mpv.desktop"];
      "audio/ogg" = ["mpv.desktop"];
      "audio/wav" = ["mpv.desktop"];
      "audio/webm" = ["mpv.desktop"];

      # Web
      "x-scheme-handler/http" = ["firefox-launcher.desktop"];
      "x-scheme-handler/https" = ["firefox-launcher.desktop"];
      "x-scheme-handler/ftp" = ["firefox-launcher.desktop"];
      "x-scheme-handler/chrome" = ["firefox-launcher.desktop"];
      "text/html" = ["firefox-launcher.desktop"];
      "application/x-extension-htm" = ["firefox-launcher.desktop"];
      "application/x-extension-html" = ["firefox-launcher.desktop"];
      "application/x-extension-shtml" = ["firefox-launcher.desktop"];
      "application/xhtml+xml" = ["firefox-launcher.desktop"];
      "application/x-extension-xhtml" = ["firefox-launcher.desktop"];
      "application/x-extension-xht" = ["firefox-launcher.desktop"];
    };
  };
}
