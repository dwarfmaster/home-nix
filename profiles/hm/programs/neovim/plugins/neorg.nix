{ pkgs, config, lib, ... }:

let
  inherit (lib) mkEnableOption mkOption types;
  cfg = config.plugins.neorg;
in {
  options.plugins.neorg = {
    enable = mkEnableOption "neorg";
  };

  config = lib.mkIf cfg.enable {
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
}
