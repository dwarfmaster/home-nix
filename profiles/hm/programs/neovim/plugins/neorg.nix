{ pkgs, config, lib, ... }:

let
  inherit (lib) mkEnableOption mkOption types;
  cfg = config.programs.nixvim.plugins.neorg;
in {
  options.programs.nixvim.plugins.neorg = {
    enable = mkEnableOption "neorg";
  };

  config = lib.mkIf cfg.enable {
    programs.nixvim = {
      extraPlugins = [ pkgs.vimPlugins.neorg ];
      extraConfigLua = ''
      -- Neorg {{{
      require("neorg").setup {
        load = {
          ["core.defaults"] = {}
        }
      }
      -- }}}
      '';
    };
  };
}
