{
  pkgs,
  ...
}: {
  home.packages = [
    # Player
    pkgs.mpv # video player

    # Conversion
    # pkgs.ffmpeg-full # Convert any video/audio format to any other

    # Downloader
    pkgs.yt-dlp # Video downloader
  ];
}
