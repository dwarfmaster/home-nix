{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.programs.why3;

  provers = import ./provers.nix;

  proversType = types.attrsOf (types.submodule (
    { name, config, ... }: {
      options = {
        type = mkOption {
          type = types.enum [ "z3" "cvc4" "alt-ergo" "coq" ];
          description = ''
            The kind of prover that is being added to why3.
          '';
        };
        package = mkOption {
          type = types.package;
          description = ''
            The package of the prover.
          '';
        };
        driver = mkOption {
          type = types.str;
          description = ''
            The driver used by why3 to interact with the prover. The default
            value is correct if the package is the why3 version of the prover.
          '';
        };
        editor = mkOption {
          type = types.str;
          default = "";
          description = ''
            The editor used to edit the input to the prover.
          '';
        };
        shortcut = mkOption {
          type = types.str;
          default = "";
          internal = true;
        };
      };

      config = {
        shortcut = name;
        driver = mkDefault provers.${config.type}.defaultDriver;
        editor = mkDefault provers.${config.type}.defaultEditor;
      };
    }
  ));

  why3Module = {
    enable = mkEnableOption "Why3 - A platform for deductive program verification";
    package = mkOption {
      type = types.package;
      default = pkgs.why3;
      description = ''
        The Why3 package to use.
      '';
    };
    provers = mkOption {
      type = proversType;
      default = { };
      description = ''
        The provers to enable for Why3.
      '';
    };
    fontSize = mkOption {
      type = types.int;
      default = 12;
      description = ''
        The size of the font in the program.
      '';
    };
    darkTheme = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Tweaks the colors of the color scheme of the source code to make it more
        readable when using a dark GTK theme.
      '';
    };
    magic = mkOption {
      type = types.int;
      default = 14;
    };
    memlimit = mkOption {
      type = types.int;
      default = 1000;
      description = ''
        The maximum amount of memory in ko allowed to each prover.
      '';
    };
    timelimit = mkOption {
      type = types.int;
      default = 5;
      description = ''
        The maximum amount of time in seconds allowed to each prover.
      '';
    };
  };

in {

  options.programs.why3 = why3Module;

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    home.file.".why3.conf".text = (''
[main]
default_editor = "vim %f"
magic = ${toString cfg.magic}
memlimit = ${toString cfg.memlimit}
running_provers_max = 2
timelimit = ${toString cfg.timelimit}

[ide]
font_size = ${toString cfg.fontSize}
''

+ (if cfg.darkTheme then (import ./dark-theme.nix) else "")

+ concatStrings (mapAttrsToList
    (n: v: "[prover]\n" + provers.${v.type}.config v + "\n")
    cfg.provers));
  };
}
