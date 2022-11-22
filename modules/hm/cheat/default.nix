{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit
    (lib)
    mkEnableOption
    mkOption
    types
    mapAttrsToList
    ;
  inherit (builtins) concatStringsSep;
  cfg = config.programs.cheat;

  cheat = types.submodule ({config, ...}: {
    options = {
      tags = mkOption {
        type = types.listOf types.str;
        description = "Tags to add to the cheatsheet";
        default = [];
      };
      syntax = mkOption {
        type = types.nullOr types.str;
        description = "The syntax of the cheatsheet";
        default = null;
      };
      text = mkOption {
        type = types.nullOr types.str;
        description = "The content of the cheatsheet";
        default = null;
      };
      source = mkOption {
        type = types.nullOr types.path;
        description = "The content of the cheatsheet in a separate file";
        default = null;
      };
    };
  });

  cheatpath = types.submodule ({config, ...}: {
    options = {
      name = mkOption {
        type = types.str;
        description = "Name for the cheatsheet database";
      };
      path = mkOption {
        type = types.str;
        description = "Path for the cheatsheet database";
      };
      readonly = mkOption {
        type = types.bool;
        description = "Whether the cheatsheets inside can be edited";
        default = true;
      };
      tags = mkOption {
        type = types.listOf types.str;
        description = "Tags to apply to all cheatsheets inside";
        default = [];
      };
    };
  });

  mkCheatSheet = name: cheat:
    pkgs.writeTextFile {
      name = "cheat-${name}";
      destination = "/${name}";
      text =
        ''
          ---
          ${
            if !(builtins.isNull cheat.syntax)
            then "syntax: ${cheat.syntax}"
            else ""
          }
          tags: [ ${concatStringsSep ", " cheat.tags} ]
          ---
        ''
        + (
          if builtins.isNull cheat.text
          then builtins.readFile cheat.source
          else cheat.text
        );
    };
  personal-src = pkgs.symlinkJoin {
    name = "cheat-personal";
    paths = mapAttrsToList mkCheatSheet cfg.cheats;
  };
  personal = {
    name = "personal";
    path = "${personal-src}";
    readonly = true;
    tags = ["personal"];
  };

  community-src = pkgs.callPackage ./community.nix {};
  community = {
    name = "community";
    path = "${community-src}";
    readonly = true;
    tags = ["community"];
  };
  cheatpaths =
    [personal]
    ++ cfg.cheatpaths
    ++ (
      if cfg.enableCommunity
      then [community]
      else []
    );

  config-content =
    cfg.extraConfig
    // {
      inherit cheatpaths;
    };
  config-file =
    pkgs.writeText "cheat-conf.yml"
    (builtins.toJSON config-content);
in {
  options = {
    programs.cheat = {
      enable = mkEnableOption "Cheatsheets for the command line";
      enableCommunity = mkOption {
        type = types.bool;
        default = true;
        description = "Install the community cheatsheets";
      };
      cheats = mkOption {
        type = types.attrsOf cheat;
        default = {};
        description = "Additional cheatsheets added to a personal database";
      };
      cheatpaths = mkOption {
        type = types.listOf cheatpath;
        default = [];
        description = "Cheatpaths to add";
      };
      extraConfig = mkOption {
        type = types.attrs;
        default = {
          editor = "vim";
          colorize = true;
          style = "monokai";
          formatter = "terminal256";
          pager = "${pkgs.less}/bin/less -FRX";
        };
        description = "Additional config";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [pkgs.cheat];
    home.sessionVariables = {
      CHEAT_CONFIG_PATH = "${config-file}";
    };
  };
}
