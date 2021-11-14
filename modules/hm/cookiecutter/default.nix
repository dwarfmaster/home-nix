{ config, lib, pkgs, ... }:

let

  inherit (lib)
    mkEnableOption mkOption types mkIf mkMerge
    range concatMapStrings concatStringsSep mapAttrsToList
    mapAttrs' nameValuePair;
  inherit (builtins) isAttrs;

  cfg = config.programs.cookiecutter;

  mkTabs = tabs: concatMapStrings (_: "  ") (range 1 tabs);
  formatAttr = tabs: attr:
    concatStringsSep "\n"
      (mapAttrsToList (name: value: mkTabs tabs + name + ":" + (if isAttrs value then "\n" + formatAttr (tabs+1) value else " \"" + value + "\"")) attr);
  cookiecutterCfgFormat = attrs: formatAttr 0 attrs + "\n";

in {
  options = {
    programs.cookiecutter = {
      enable = mkEnableOption "CookieCutter - a project template system";

      package = mkOption {
        description = "cookiecutter package to use";
        type = types.package;
        default = pkgs.cookiecutter;
      };

      configFile = mkOption {
        description = "Path to the config file";
        type = types.path;
        default = "${config.xdg.configHome}/cookiecutterrc";
      };

      extraConfig = mkOption {
        description = "Content of the config file";
        type = types.attrs;
        default = { };
        internal = true;
      };

      templates = mkOption {
        description = "Pre-installed templates";
        type = types.attrsOf (types.either types.package types.path);
        default = { };
      };

      templateDir = mkOption {
        description = "Directory to store templates";
        type = types.path;
        default = "${config.xdg.dataHome}/cookiecutters/templates";
      };

      replayDir = mkOption {
        description = "Directory to store replays";
        type = types.path;
        default = "${config.xdg.stateHome}/cooliecutters/replays";
      };

      defaults = mkOption {
        description = "Default values for prompts";
        type = types.attrsOf types.str;
        default = { };
      };

      aliases = mkOption {
        description = "Aliases for templates";
        type = types.attrsOf types.str;
        default = { };
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home.packages = [ cfg.package ];
      home.sessionVariables = {
        COOKIECUTTER_CONFIG = "${cfg.configFile}";
      };
      programs.cookiecutter.extraConfig = {
        cookiecutters_dir = "${cfg.templateDir}";
        replay_dir = "${cfg.replayDir}";
        default_context = cfg.defaults;
        abbreviations = cfg.aliases;
      };

      home.file =
        mapAttrs'
          (name: value: nameValuePair "${cfg.templateDir}/${name}" {
            source = "${value}";
            recursive = false;
          })
          cfg.templates;
    })

    (mkIf (cfg.enable && !(isNull cfg.extraConfig)) {
      home.file."${cfg.configFile}".text = cookiecutterCfgFormat cfg.extraConfig;
    })
  ];
}
