{ config, pkgs, ... }:

{
  xsession.windowManager.bspwm = {
    enable = true;
    monitors."eDP-1" = [
      "l4" "l3" "l2" "l1"
      "music" "misc"
      "r1" "r2" "r3" "r4"
    ];
    settings = {
      border_width = 2;
      window_gap = 10;
      split_ratio = 0.52;
      borderless_monocle = true;
      gapless_monocle = true;
    };
  };

  services.sxhkd.keybindings = {
    # Reload config file
    "super + Escape" = "pkill -USR1 -x sxhkd";
    "super + shift n" = "bspc quit";
    "super + x" = "bspc node -c";
    # Alternate between tiled and monocle layouts
    "super + a" = "bspc desktop -l next";
    "super + shift + Enter" = "bspc node -s biggest.window";
    # Preselect direction
    "super + alt + {h,j,k,l}" = "bspc node -p {west,south,north,east}";
    "super + {_,shift + }Tab" = "bspc node -f {next,prev}.local.!hidden.window";
    "super + {_,shift + }{q,s,d,f,g,h,j,k,l,m}" =
      "bspc {desktop -f,node -d} '^{1,2,3,4,5,6,7,8,9,10}'";
  };
}
