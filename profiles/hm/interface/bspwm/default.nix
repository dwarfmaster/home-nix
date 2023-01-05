{
  config,
  pkgs,
  ...
}: let
  colors = config.colorScheme.colors;
in {
  xsession.windowManager.bspwm = {
    enable = true;
    monitors."eDP-1" = [
      "l4"
      "l3"
      "l2"
      "l1"
      "music"
      "misc"
      "r1"
      "r2"
      "r3"
      "r4"
    ];
    settings = {
      # Borders
      normal_border_color = "#${colors.base01}";
      active_border_color = "#${colors.base03}";
      focused_border_color = "#${colors.base0A}";
      presel_feedback_color = "#${colors.base0A}";
      border_width = 2;
      # Spacing
      window_gap = 5;
      split_ratio = 0.52;
      automatic_scheme = "alternate";
      initial_polarity = "second_child";
      presel_feedback = true;
      borderless_monocle = true;
      gapless_monocle = true;
      single_monocle = false; # Use monocle when there is only one window
      # Pointer
      pointer_modifier = "mod1";
      pointer_action1 = "move";
      pointer_action2 = "resize_side";
      pointer_action3 = "resize_corner";
      click_to_focus = "any";
      swallow_first_click = false;
      pointer_follows_focus = true;
      pointer_follows_monitor = true;
      focus_follows_pointer = true;
      # EWMH
      ignore_ewmh_focus = false;
      ignore_ewmh_fullscreen = false;
      ignore_ewmh_struts = false;
      center_pseudo_tiled = true;
      honor_size_hints = false;
      # Monitors
      remove_disabled_monitors = true;
      remove_unplugged_monitors = true;
      merge_overlapping_monitors = true;
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
    "super + {_,shift + }{q,s,d,f,g,h,j,k,l,m}" = "bspc {desktop -f,node -d} '^{1,2,3,4,5,6,7,8,9,10}'";
  };

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
