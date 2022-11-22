{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.applications;

  inherit (lib) mkOption types;

  mkAppOption = desc:
    mkOption {
      type = types.str;
      description = "A path to " ++ desc ++ ".";
    };
in {
  options = {
    # Settings to set global applications that may be referred from many places
    applications = {
      terminal = mkAppOption "a terminal emulator";
      locker = mkAppOption "a graphical screen locker";
      launcher = mkAppOption "an app launcher";
      calculator = mkAppOption "a graphical calculator";
      browser = mkAppOption "an internet browser";
      volume = mkAppOption "a volume manager. Must accept up, down, toggle, mute and unmute arguments";
      brightness = mkAppOption "a brightness manager. Must accept up, down, dim, undim and toggle arguments";
      media = mkAppOption "a media manager. Must accept next, prev, toggle, play and pause arguments";
      notifier = mkAppOption "a notifier. It must take 3 arguments : a priority (low, normal, high, progress), a title and a message, or just clear. If the priority is progress the message must be an integer";
    };
  };
}
