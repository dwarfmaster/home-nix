{ pkgs, ... }:
{
  services.picom = {
    enable = true;
    package = pkgs.picom-next;
    backend = "glx";
    vSync = true;

    fade = true;
    fadeDelta = 3;

    shadow = false;

    activeOpacity = 1.0;
    inactiveOpacity = 0.9;
    menuOpacity = 0.9;

    extraArgs = [
      "--corner-radius=10"
      "--blur-method=dual_kawase"
      "--blur-strength=7"
    ];
  };
}
