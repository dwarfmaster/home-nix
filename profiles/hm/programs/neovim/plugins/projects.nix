{ config, pkgs, lib, ... }:

let
  inherit (lib) mkEnableOption mkOption types;
  cfg = config.plugins.project-nvim;
in {
  options.plugins.project-nvim = {
    enable = mkEnableOption "project.nvim plugin";
    methods = mkOption {
      description = lib.mdDoc "Methods of detecting the root directory";
      # TODO is type really appropriate ? 
      type = types.listOf types.str;
      default = [ "lsp" "pattern" ];
    };
    patterns = mkOption {
      description = lib.mdDoc "Patterns used to detect root dir";
      type = types.listOf types.str;
      default = [ ".git" "_darcs" ".hg" ".bzr" ".svn" "flake.nix" ];
    };
    ignore_lsp = mkOption {
      description = lib.mdDoc "LSP clients to ignore";
      type = types.listOf types.str;
      default = [ ];
      example = [ "null-ls" ];
    };
    manual = mkOption {
      description = lib.mdDoc "Manually change project directory";
      type = types.bool;
      default = false;
    };
    silent = mkOption {
      description = lib.mdDoc "Do not message when changing chdir";
      type = types.bool;
      default = true;
    };
  };

  config = let
    printB = b: if b then "true" else "false";
    printL = l: "{ " + lib.concatMapStringsSep ", " (v: "\"${v}\"") l + " }";
  in lib.mkMerge [
      (lib.mkIf cfg.enable {
        extraPlugins = [ pkgs.vimPlugins.project-nvim ];
        extraConfigLua = ''
          -- project.nvim setup {{{
          require("project_nvim").setup {
            manual_mode = ${printB cfg.manual},
            detection_methods = ${printL cfg.methods},
            patterns = ${printL cfg.patterns},
            ignore_lsp = ${printL cfg.ignore_lsp},
            silent_chdir = ${printB cfg.silent},
          }
          -- }}}
        '';
    })
    (lib.mkIf (cfg.enable && config.plugins.telescope.enable) {
      plugins.telescope.enabledExtensions = [ "projects" ];
    })
  ];
}
