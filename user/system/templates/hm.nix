{ config, lib, pkgs, ... }:

let

  inherit (lib)
    mkEnableOption mkOption types mkIf mkMerge
    range concatMapStrings concatStringsSep mapAttrsToList;
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
        # TODO improve type
        type = types.nullOr types.attrs;
        default = null;
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home.packages = [ cfg.package ];
      home.sessionVariables = {
        COOKIECUTTER_CONFIG = "${cfg.configFile}";
      };
    })

    (mkIf (cfg.enable && !(isNull cfg.extraConfig)) {
      home.file."${cfg.configFile}".text = cookiecutterCfgFormat cfg.extraConfig;
    })
  ];
}
