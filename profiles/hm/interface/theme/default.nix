{
  pkgs,
  config,
  ...
}: {
  # Set the wallpaper using feh
  xsession.initExtra = ''
    ${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${config.stylix.image}
  '';

  # For some reason this is needed:
  # See: https://github.com/danth/stylix/issues/499
  stylix.targets.hyprland.enable = false;
}
