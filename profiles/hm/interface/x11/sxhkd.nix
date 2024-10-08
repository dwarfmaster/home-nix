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
      "super + p" = "${apps.locker}";
      "super + equal" = "${apps.calculator}";
      "{XF86AudioRaiseVolume,XF86AudioLowerVolume,XF86AudioMute" = "${apps.volume} {up,down,toggle}";
      "{XF86MonBrightnessUp,XF86MonBrightnessDown}" = "${apps.brightness} {up,down}";
    };
  };
}
