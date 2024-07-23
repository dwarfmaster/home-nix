{
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.android-tools
    pkgs.android-udev-rules
  ];
}
