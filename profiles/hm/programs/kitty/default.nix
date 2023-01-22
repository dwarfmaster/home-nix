{ config, pkgs, lib, ... }:

let
  template = pkgs.fetchFromGitHub {
    owner = "kdrag0n";
    repo = "base16-kitty";
    rev = "06bb401fa9a0ffb84365905ffbb959ae5bf40805";
    sha256 = "sha256-aRaizTYPpuWEcvoYE9U+YRX+Wsc8+iG0guQJbvxEdJY=";
  };
  colors = lib.mapAttrs' (name: v: lib.nameValuePair "${name}-hex" v) config.colorScheme.colors;
  values =
    colors
    // {
      scheme-name = config.colorScheme.slug;
      scheme-author = "???";
    };
  themeFile =
    config.lib.mustache.render
    "colors.conf"
    "${template}/templates/default-256.mustache"
    values;
in {
  programs.kitty = {
    enable = true;
    font = {
      name = "FiraCode Nerd Font Mono";
      size = 12;
    };
    extraConfig = builtins.readFile themeFile;
    settings = {
      force_ltr = true;
      disable_ligatures = "cursor";
      cursor_shape = "block";
      cursor_blink_interval = 0;

      scrollback_lines = 10000;
      sync_to_monitor = true;
      enable_audio_bell = false;
      tab_bar_style = "hidden";

      allow_remote_control = "socket-only";
      listen_on = "unix:\${XDG_RUNTIME_DIR}/kitty";

      shell_integration = "no-cursor";
    };
  };

  applications.terminal = "${config.programs.kitty.package}/bin/kitty -1";
}
