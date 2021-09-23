{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = [
    # Player
    pkgs.mpv                         # video player

    # Conversion
    pkgs.ffmpeg-full                 # Convert any video/audio format to any other

    # Downloader
    pkgs.python38Packages.youtube-dl # Video downloader
  ];
}
