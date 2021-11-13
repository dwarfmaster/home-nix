{ config, lib, ... }:

# Spec: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
# Mostly handled by home manager out of the box
# - XDG_DATA_HOME: handled by home-manager
# - XDG_CONFIG_HOME: handled by home-manager
# - XDG_STATE_HOME: not yet handled by home-manager, but already upstream
# - .local/bin is meaningfully NOT in path, for security reasons
# - XDG_DATA_DIRS: handled by system config
# - XDG_CONFIG_DIRS: handled by system config
# - XDG_CACHE_HOME: handled by home-manager
# - XDG_RUNTIME_DIR: handled by system config

let
  inherit (lib) mkOption types;
  cfg = config.xdg;
in {
  options = {
    xdg.stateHome = mkOption {
      type = types.path;
      defaultText = "~/.local/state";
      apply = toString;
      description = ''
        Absolute path to directory holding application states.
      '';
    };
  };

  config = {
    xdg.enable = true;
    xdg.stateHome = "${config.home.homeDirectory}/.local/state";

    home.sessionVariables = {
      XDG_STATE_HOME = cfg.stateHome;
    };
  };
}
