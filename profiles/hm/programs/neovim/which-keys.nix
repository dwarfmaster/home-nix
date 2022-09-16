{ config, pkgs, lib, ... }:

let

  inherit (lib)
    mkIf mkMerge mkOption mkEnableOption types;

  cfg = config.programs.nixvim.plugins.which-key;

  mkSelect = default: name: mkOption {
    inherit default;
    description = lib.mdDoc "Wether to enable ${name}";
    type = types.bool;
  };

  printB = b: if b then "true" else "false";

  renderList = lib.concatMapStringsSep ", " (v: "\"${v}\"");

  bindingType = types.submodule ({ config, ...}: {
    options = {
      binding = mkOption {
        description = lib.mdDoc "The binding";
        type = types.nullOr types.str;
        example = "<cmd>Telescope find_files<cr>";
        default = null;
      };
      cmd = mkOption {
        description = lib.mdDoc "The command to execute. Will set binding.";
        type = types.nullOr types.str;
        example = "Telescope find_files";
        default = null;
      };
      lua = mkOption {
        description = lib.mdDoc "Lua code to execute. Will set binding.";
        type = types.nullOr types.str;
        example = "print(\"bar\")";
        default = null;
      };
      description = mkOption {
        description = lib.mdDoc "The description of the binding.";
        type = types.str;
        example = "Find files in project";
      };
      silent = mkOption {
        description = lib.mdDoc "Use `silent` when creating binding.";
        type = types.nullOr types.bool;
        default = null;
      };
      noremap = mkOption {
        description = lib.mdDoc "Use `noremap` when creating binding.";
        type = types.nullOr types.bool;
        default = null;
      };
      buffer = mkOption {
        description = lib.mdDoc "Create binding only for specific buffer.";
        type = types.nullOr types.ints.positive;
        default = null;
      };
    };

    config = mkMerge [
      (mkIf (!(builtins.isNull config.cmd)) { binding = "<cmd>${config.cmd}<cr>"; })
      (mkIf (!(builtins.isNull config.lua)) { binding = "function() ${config.lua} end"; })
    ];
  });

  bindingTreeType = types.submodule ({ config, ...}: {
    options = {
      name = mkOption {
        description = "Name of the category";
        type = types.nullOr types.str;
        default = null;
      };
      bindings = mkOption {
        description = "Bindings at this level";
        type = types.attrsOf bindingType;
        default = { };
      };
      subs = mkOption {
        description = "Sub-trees";
        type = types.attrsOf bindingTreeType;
        default = { };
      };
    };
  });

  mergeAll = lib.foldr (as1: as2: as1 // as2) {};
  prepareMapping = opts: prefix: key: bd: 
    let
      nullOr = v1: v2: if builtins.isNull v1 then v2 else v1;
      inherit (bd) description binding;
      silent = nullOr bd.silent opts.silent;
      noremap = nullOr bd.noremap opts.noremap;
      buffer = nullOr bd.buffer opts.buffer;
    in lib.nameValuePair (prefix + key) (
      { inherit binding description silent noremap; }
      // (if builtins.isNull buffer then {} else { inherit buffer; })
      // { lua = !(builtins.isNull bd.lua); });
  accumulateMapping = opts: prefix: name: tree:
    let
      category = { "${prefix}" = if builtins.isNull tree.name then name else tree.name; };
      bindings = lib.mapAttrs' (prepareMapping opts prefix) tree.bindings;
      subs = lib.mapAttrsToList (k: accumulateMapping opts (prefix + k) k) tree.subs;
    in mergeAll ([ category bindings ] ++ subs);
  mappings =
    let
      opts = { silent = true; noremap = true; buffer = null; };
    in lib.mapAttrs (_: maps: mergeAll (lib.mapAttrsToList (key: accumulateMapping opts key key) maps)) cfg.bindings;

  escapeCmd = m: 
    if m.lua then m.binding
    else "\"" + builtins.replaceStrings [ "\"" ] [ "\\\"" ] m.binding + "\"";
  renderMapping = m:
    if builtins.isString m
    then "{ name = \"${m}\" }" 
    else (if builtins.isNull m.binding 
      then "\"${m.description}\""
      else "{ ${escapeCmd m}, \"${m.description}\", " 
         + "silent=${printB m.silent}, noremap=${printB m.noremap}"
         + (if m ? "buffer" then ", buffer${toString m.buffer} }" else "}"));
  renderMappings = mode: mappings: ''
      require('which-key').register({
        ${lib.concatStringsSep ",\n  "
            (lib.mapAttrsToList (k: m: "[\"${k}\"] = ${renderMapping m}") mappings)}
      }, { mode = "${mode}" })
    '';
  renderedMappings = lib.concatStringsSep "\n" (lib.mapAttrsToList renderMappings mappings);

in {
  options.programs.nixvim.plugins.which-key = {
    enable = mkEnableOption "which-key.nvim plugin";

    plugins = {
      marks = mkSelect true "completion for marks";
      registers = mkSelect true "completion for registers";
      spelling = {
        enable = mkSelect false "completion for spelling";
        suggestions = mkOption {
          default = 20;
          type = types.int;
          description = lib.mdDoc "Number of spelling suggestions to show";
        };
      };
      presets = {
        enable = mkSelect true "completion for default keybindings";
        operators = mkSelect cfg.plugins.presets.enable "completion for operators";
        motions = mkSelect cfg.plugins.presets.enable "completion for motions";
        text_objects = mkSelect cfg.plugins.presets.enable "completion for text objects";
        windows = mkSelect cfg.plugins.presets.enable "completion for windows";
        nav = mkSelect cfg.plugins.presets.enable "completion for navigation";
        z = mkSelect cfg.plugins.presets.enable "completion for bindings beggining with z";
        g = mkSelect cfg.plugins.presets.enable "completion for bindings beggining with g";
      };
    };

    operators = mkOption {
      description = lib.mdDoc "Operators to trigger motion and text object completion for";
      type = types.attrsOf types.str;
      default = { };
      example = { gc = "Comments"; };
    };

    labels = mkOption {
      description = lib.mdDoc "Change label used to display some keys";
      type = types.attrsOf types.str;
      default = { };
      example = { "<spaces>" = "SPC"; "<cr>" = "RET"; };
    };
    
    icons = {
      breadcrumb = mkOption {
        description = lib.mdDoc "Symbol used to show active key combo";
        type = types.str;
        default = "»";
      };
      separator = mkOption {
        description = lib.mdDoc "Symbol used between a key and its label";
        type = types.str;
        default = "➜";
      };
      group = mkOption {
        description = lib.mdDoc "Symbol prepended to a group";
        type = types.str;
        default = "+";
      };
    };

    popup = {
      scroll = {
        up = mkOption {
          description = lib.mdDoc "Binding to scroll up inside the popup";
          type = types.str;
          default = "<c-u>";
        };
        down = mkOption {
          description = lib.mdDoc "Binding to scroll down inside the popup";
          type = types.str;
          default = "<c-d>";
        };
      };

      window = {
        border = mkOption {
          description = lib.mdDoc "The border of the popup window";
          type = types.enum [ "none" "single" "double" "shadow" ];
          default = "none";
        };
        position = mkOption {
          description = lib.mdDoc "Position of the popup window";
          type = types.enum [ "bottom" "top" ];
          default = "bottom";
        };
        margin = let 
          mkMargin = def: desc: mkOption {
            default = def;
            description = lib.mdDoc "${desc} window margin";
            type = types.int;
          };
        in {
          top = mkMargin 1 "Top";
          bottom = mkMargin 1 "Bottom";
          left = mkMargin 0 "Left";
          right = mkMargin 0 "Right";
        };
        padding = let 
          mkPadding = def: desc: mkOption {
            default = def;
            description = lib.mdDoc "${desc} window padding";
            type = types.int;
          };
        in {
          top = mkPadding 2 "Top";
          bottom = mkPadding 2 "Bottom";
          left = mkPadding 2 "Left";
          right = mkPadding 2 "Right";
        };
        blend = mkOption {
          description = lib.mdDoc "Blend from total opacity (0) to transparency (100)";
          type = types.ints.between 0 100;
          default = 0;
        };
      };

      layout = let 
        mkMinMax = m: cat: def: mkOption {
          description = lib.mdDoc "${m} ${cat} of the columns";
          type = types.ints.positive;
          default = def;
        };
      in {
        height = {
          min = mkMinMax "Min" "height" 4;
          max = mkMinMax "Max" "height" 25;
        };
        width = {
          min = mkMinMax "Min" "width" 20;
          max = mkMinMax "Max" "width" 50;
        };
        spacing = mkOption {
          description = lib.mdDoc "Spacing between columns";
          default = 3;
          type = types.ints.positive;
        };
        align = mkOption {
          description = lib.mdDoc "Align columns";
          default = "left";
          type = types.enum [ "left" "center" "right" ];
        };
      };
    };

    missing = mkOption {
      description = lib.mdDoc "Show mappings without a label";
      default = true;
      type = types.bool;
    };
    hidden = mkOption {
      description = lib.mdDoc "Hide mapping boilerplate";
      type = types.listOf types.str;
      default = [ "<silent>" "<cmd>" "<Cmd>" "<CR>" "call" "lua" "^:" "^ " ];
    };
    help = mkOption {
      description = lib.mdDoc "Show help messages when popup is visible";
      type = types.bool;
      default = true;
    };
    triggers = {
      setup = mkOption {
        description = lib.mdDoc "Prefixes hooked by which-key";
        type = types.either (types.enum [ "auto" ]) (types.listOf types.str);
        default = "auto";
        example = [ "<leader>" ];
      };
      blacklist = mkOption {
        description = lib.mdDoc "Modes + prefixes that shouldn't be hooked by which-key";
        type = types.attrsOf (types.listOf types.str);
        default = {
          i = [ "j" "k" ];
          v = [ "j" "k" ];
        };
      };
    };
    disable = {
      buftypes = mkOption {
        description = lib.mdDoc "Buffer types to disable which-key in";
        type = types.listOf types.str;
        default = [];
      };
      filetypes = mkOption {
        description = lib.mdDoc "File types to disable which-key in";
        type = types.listOf types.str;
        default = [ "TelescopePromp" ];
      };
    };

    bindings = let
      mkBindings = name: mkOption {
        description = lib.mdDoc "Bindings for ${name} mode.";
        type = types.attrsOf bindingTreeType;
        default = {};
      };
    in {
      n = mkBindings "normal";
      i = mkBindings "insert";
      v = mkBindings "visual";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      { assertion = cfg.popup.layout.height.min <= cfg.popup.layout.height.max
      ; message = "which-key.nvim max column height must be greater than min column height"; }
      { assertion = cfg.popup.layout.width.min <= cfg.popup.layout.width.max
      ; message = "which-key.nvim max column width must be greater than min column width"; }
    ];

    programs.nixvim = {
      extraPlugins = [ pkgs.vimPlugins.which-key-nvim ];
      extraConfigLua = let 
        triggers =
          if builtins.isString cfg.triggers.setup
          then "\"${cfg.triggers.setup}\""
          else "{ " + lib.concatMapStringsSep ", " (v: "\"${v}\"") cfg.triggers.setup + " }";
      in ''
        -- Set up which-keys {{{
        require('which-key').setup({
          plugins = {
            marks = ${printB cfg.plugins.marks},
            registers = ${printB cfg.plugins.registers},
            spelling = {
              enabled = ${printB cfg.plugins.spelling.enable},
              suggestions = ${toString cfg.plugins.spelling.suggestions},
            },
            presets = {
              operators = ${printB cfg.plugins.presets.operators},
              motions = ${printB cfg.plugins.presets.motions},
              text_objects = ${printB cfg.plugins.presets.text_objects},
              windows = ${printB cfg.plugins.presets.windows},
              nav = ${printB cfg.plugins.presets.nav},
              z = ${printB cfg.plugins.presets.z},
              g = ${printB cfg.plugins.presets.g},
            },
          },
          operators = { ${lib.concatStringsSep ", " (lib.mapAttrsToList (n: v: "${n} = \"${v}\"") cfg.operators)} },
          key_labels = {
            ${lib.concatStringsSep ",\n" (lib.mapAttrsToList (n: v: "[\"${n}\"] = \"${v}\"") cfg.labels)}
          },
          icons = {
            breadcrumb = "${cfg.icons.breadcrumb}",
            separator = "${cfg.icons.separator}",
            group = "${cfg.icons.group}",
          },
          popup_mappings = {
            scroll_down = '${cfg.popup.scroll.down}',
            scroll_up = '${cfg.popup.scroll.up}',
          },
          window = {
            border = "${cfg.popup.window.border}",
            position = "${cfg.popup.window.position}",
            margin = { ${let m = cfg.popup.window.margin; in 
                           "${toString m.top}, ${toString m.right}, ${toString m.bottom}, ${toString m.left}"} },
            padding = { ${let m = cfg.popup.window.padding; in 
                            "${toString m.top}, ${toString m.right}, ${toString m.bottom}, ${toString m.left}"} },
            winblend = ${toString cfg.popup.window.blend}
          },
          layout = {
            height = { min = ${toString cfg.popup.layout.height.min}, max = ${toString cfg.popup.layout.height.max} },
            width = { min = ${toString cfg.popup.layout.width.min}, max = ${toString cfg.popup.layout.width.max} },
            spacing = ${toString cfg.popup.layout.spacing},
            align = "${cfg.popup.layout.align}",
          },
          ignore_missing = ${printB (!cfg.missing)},
          hidden = { ${renderList cfg.hidden}},
          show_help = ${printB cfg.help},
          triggers = ${triggers},
          triggers_blacklist = {
            ${lib.concatStringsSep ",\n    "
               (lib.mapAttrsToList
                 (n: v: "${n} = { ${renderList v} }")
                 cfg.triggers.blacklist)}
          },
          disable = {
            buftypes = { ${renderList cfg.disable.buftypes} },
            filetypes = { ${renderList cfg.disable.filetypes} },
          },
        })

        --- Keybindings {{{
        ${renderedMappings}
        --- }}}

        --- }}}
      '';
    };
  };
}
