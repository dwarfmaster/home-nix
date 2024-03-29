{
  config,
  pkgs,
  ...
}: let
  bspdesk-bin = pkgs.writeShellScriptBin "bspdesk" (builtins.readFile ./bspdesk.sh);
  bspdesk = "${bspdesk-bin}/bin/bspdesk";
in {
  home.packages = [bspdesk-bin];
  xsession.windowManager.bspwm = {
    enable = true;
    extraConfigEarly = ''
      ${bspdesk} create-workspace personal
      ${bspdesk} create-workspace settings
      bspc desktop "personal^r1" -f
      bspc desktop Desktop -r
    '';
    extraConfig = ''
      ${config.programs.eww.package}/bin/eww open-many \
          statusbar-left \
          statusbar-center \
          statusbar-right
    '';
    settings = {
      # Borders
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
    rules = {
      # For some reason this is necessary
      "Zathura".state = "tiled";
    };
  };
  stylix.targets.bspwm.enable = true;

  services.sxhkd.keybindings = {
    # Reload config file
    "super + Escape" = "pkill -USR1 -x sxhkd";
    "super + shift n" = "bspc quit";
    "super + x" = "bspc node -c";
    # Alternate between tiled and monocle layouts
    "super + {a,z}" = "bspc desktop -l {monocle,tiled}";
    # Preselect direction
    "super + alt + {h,j,k,l}" = "bspc node -p {west,south,north,east}";
    "super + {_,shift + }Tab" = "bspc node -f {next,prev}.local.!hidden.window";
    # Switch workspace
    "super + {_,shift + }{q,s,d,f,g,h,j,k,l,m}" = "${bspdesk} {focus,send-to} '{l4,l3,l2,l1,l0,r0,r1,r2,r3,r4}'";
    "super + {_,shift + }space" = "${bspdesk} {focus-select,send-to-select}";
    "super + {_,shift + }c" = "${bspdesk} {create-select,remove-select}";
  };
}
