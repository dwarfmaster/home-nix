{
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
      "x-scheme-handler/http"         = [ "firefox.desktop" ];
      "x-scheme-handler/https"        = [ "firefox.desktop" ];
      "x-scheme-handler/ftp"          = [ "firefox.desktop" ];
      "x-scheme-handler/chrome"       = [ "firefox.desktop" ];
      "text/html"                     = [ "firefox.desktop" ];
      "application/x-extension-htm"   = [ "firefox.desktop" ];
      "application/x-extension-html"  = [ "firefox.desktop" ];
      "application/x-extension-shtml" = [ "firefox.desktop" ];
      "application/xhtml+xml"         = [ "firefox.desktop" ];
      "application/x-extension-xhtml" = [ "firefox.desktop" ];
      "application/x-extension-xht"   = [ "firefox.desktop" ];
    };
  };
}

