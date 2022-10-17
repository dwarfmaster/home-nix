{ pkgs, config, lib, ... }:

let
  inherit (lib) mkEnableOption mkOption types;
  inherit (pkgs.local) lua;
  cfg = config.plugins.neorg;
in {
  options.plugins.neorg = {
    enable = mkEnableOption "neorg";
    setup = mkOption {
      type = lua.types.setup;
      description = "Neorg setup argument";
    };
  };

  config = lib.mkIf cfg.enable {
    plugins.neorg.setup.load = {
      "core.defaults" = {};
    };
    extraPlugins = [ pkgs.vimPlugins.neorg ];
    extraConfigLua = ''
    -- Neorg {{{
    require("neorg").setup {
      ${lua.nixvim.toLuaObject cfg.setup}
    }
    -- }}}
    '';
  };
}
