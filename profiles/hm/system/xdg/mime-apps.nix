{ config, lib, pkgs, ... }:

# SPEC: https://specifications.freedesktop.org/mime-apps-spec/mime-apps-spec-latest.html
# Mostly handled by home-manager
{
  xdg.mime.enable = true;
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      # Documents
      "application/pdf"  = [ "org.pwmt.zathura.desktop" ];

      # Videos
      "video/x-matroska" = [ "mpv.desktop" ];
      "video/x-msvideo"  = [ "mpv.desktop" ];
      "video/mp4"        = [ "mpv.desktop" ];

      # Pictures
      "image/jpeg"       = [ "sxiv.desktop" ];
      "image/png"        = [ "sxiv.desktop" ];
      "image/gif"        = [ "sxiv.desktop" ];
      "image/x-ms-bmp"   = [ "sxiv.desktop" ];

      # Web
      "x-scheme-handler/http"         = [ "firefox-launcher.desktop" ];
      "x-scheme-handler/https"        = [ "firefox-launcher.desktop" ];
      "x-scheme-handler/ftp"          = [ "firefox-launcher.desktop" ];
      "x-scheme-handler/chrome"       = [ "firefox-launcher.desktop" ];
      "text/html"                     = [ "firefox-launcher.desktop" ];
      "application/x-extension-htm"   = [ "firefox-launcher.desktop" ];
      "application/x-extension-html"  = [ "firefox-launcher.desktop" ];
      "application/x-extension-shtml" = [ "firefox-launcher.desktop" ];
      "application/xhtml+xml"         = [ "firefox-launcher.desktop" ];
      "application/x-extension-xhtml" = [ "firefox-launcher.desktop" ];
      "application/x-extension-xht"   = [ "firefox-launcher.desktop" ];
    };
  };
}
