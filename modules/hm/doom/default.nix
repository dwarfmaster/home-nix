{ config, lib, pkgs, ... }:

# TODO support building doom from nix

let

  inherit (lib)
    mkEnableOption mkOption types mkIf
    mapAttrs mapAttrs' mapAttrsRecursive mapAttrsToList foldl filterAttrs
    concatMapStrings concatStringsSep concatMap concatStrings nameValuePair;
  inherit (builtins) isString readFile attrValues;

  flattenAttr = attr:
    foldl (a: b: a // b) {} (attrValues attr);
  prefixNames = prefix: attrs:
    mapAttrs' (name: value: nameValuePair (prefix + name) value) attrs;

  cfg = config.programs.doom;

  elispFileType = types.submodule ({ config, ... }: {
    options = {
      text = mkOption {
        description = "The content of the elisp file";
        type = types.nullOr types.string;
        default = null;
      };
      source = mkOption {
        description = "A path to a file to copy";
        type = types.nullOr types.path;
        default = null;
      };
    };
  });
  mkElispOption = desc: mkOption {
    description = desc;
    type = types.nullOr elispFileType;
    default = null;
  };
  # TODO support unit tests
  doomModule = { name, config, ...}: {
    options = {
      # To enable it with options, set enable to false and manually add it to initModules
      enable = mkOption {
        description = "Should the module be automatically included in loaded modules";
        type = types.bool;
        default = true;
      };
      init = mkElispOption "Early loaded configuration file";
      config = mkElispOption "Main config file";
      packages = mkElispOption "Packages to install";
      autoload = mkElispOption "Setup functions to autoload";
      autoloads = mkOption {
        description = "Autoload files to include in the module";
        type = types.attrsOf elispFileType;
        default = { };
      };
      doctor = mkElispOption "Checks for module";
      cli = mkElispOption "Add new commands to doom program";
      nix = mkOption {
        description = "Creates a +nix.el file at the root with constants corresponding to nix values";
        type = types.attrsOf types.str;
        default = { };
      };
      extras = mkOption {
        description = "Create other files prefixed by + at the root";
        type = types.attrsOf elispFileType;
        default = { };
      };
    };
  };


  mkFile = content: (if !(isNull content.text) then { inherit (content) text; } else {})
                    // (if !(isNull content.source) then { inherit (content) source; } else {});
  pathName = path: (concatStringsSep "/" path) + ".el";
  elispFile = path: content: if !(isNull content) then { "${pathName path}" = mkFile content; } else {};
  makeNixEl = strs: concatStrings (mapAttrsToList (name: value: "(defconst *nix/${name}* \"${value}\")\n") strs);
  moduleFiles = prefix: name: module:
    prefixNames (prefix + name + "/")
      (flattenAttr
        ((mapAttrs (name: elispFile [ name ]) (removeAttrs module [ "enable" "nix" "autoloads" "extras" ]))
          // (mapAttrs (name: elispFile [ "autoloads" name ]) module.autoloads)
          // (if module.nix == { } then {} else { name = { "+nix.el".text = makeNixEl module.nix; }; })
          // (mapAttrs (name: elispFile [ ("+" + name) ]) module.extras)
        ));
  categoryFiles = prefix: category: modules:
    flattenAttr (mapAttrs (moduleFiles (prefix + category + "/")) modules);
  modulesFiles = prefix: flattenAttr (mapAttrs (categoryFiles prefix) cfg.modules);

  defaultModules =
    mapAttrs
      (_: modules: mapAttrsToList (name: _: name)
        (filterAttrs (_: mod: mod.enable) modules))
      cfg.modules;

  formatMod = mod:
    if isString mod
    then "    " + mod + "\n"
    else "    (" + mod.mod + concatMapStrings (arg: " +" + arg) mod.args + ")\n";
  init.el = ''
    (doom!
    ${concatStrings (mapAttrsToList (category: mods: "  :" + category + "\n" + concatMapStrings formatMod mods) cfg.initModules) }
      )
  '';
  files = {
    "init.el".text = init.el;
  } // modulesFiles "modules/";

in {
  options = {
    programs.doom = {
      enable = mkEnableOption "Doom Emacs - a configuration framework for GNU Emacs";

      initModules = mkOption {
        description = "Modules to load";
        type = types.attrsOf (types.listOf (types.either
          types.str
          (types.submodule
            (args:
              { options = {
                  mod = mkOption {
                    description = "Name of the module";
                    type = types.str;
                  };
                  args = mkOption {
                    description = "Arguments to the module";
                    type = types.listOf types.str;
                  };
                };
              })
          )));

        default = {
          config = [ { mod = "default"; args = [ "bindings" "smartparens" ]; } ];
        };

        example = {
          completion = [ "company" ];
          editor = [ { mod = "evil"; args = [ "everywhere" ]; } "file-templates" ];
          config = [ { mod = "default"; args = [ "bindings" "smartparens" ]; } ];
        };
      };

      modules = mkOption {
        description = "Additional configuration modules";
        type = types.attrsOf (types.attrsOf (types.submodule doomModule));
        default = { };
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [ { assertion = config.programs.emacs.enable; message = "Emacs must be enabled for doom-emacs"; } ];

    xdg.configFile = mapAttrs' (path: file: nameValuePair ("doom/" + path) file) files;
    programs.doom.initModules = defaultModules;
  };
}
