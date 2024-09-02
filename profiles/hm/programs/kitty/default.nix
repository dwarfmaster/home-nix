{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.kitty = {
    enable = true;
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
    };
    shellIntegration = {
      mode = "no-cursor";
      enableZshIntegration = true;
    };
  };
  stylix.targets.kitty.enable = true;
  stylix.targets.kitty.variant256Colors = true;

  applications.terminal = "${config.programs.kitty.package}/bin/kitty -1";
}
