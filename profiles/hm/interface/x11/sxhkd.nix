{
  config,
  pkgs,
  ...
}: let
  apps = config.applications;
in {
  services.sxhkd = {
    enable = true;
    keybindings = {
      "super + Return" = "${apps.terminal}";
      "super + r" = "${apps.launcher}";
      "super + c" = "${apps.calculator}";
      "super + p" = "${apps.locker}";
      "{XF86AudioRaiseVolume,XF86AudioLowerVolume,XF86AudioMute" = "${apps.volume} {up,down,toggle}";
      "{XF86MonBrightnessUp,XF86MonBrightnessDown}" = "${apps.brightness} {up,down}";
    };
  };
}
