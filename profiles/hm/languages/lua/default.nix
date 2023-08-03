{
  config,
  lib,
  pkgs,
  ...
}: let
  lua = pkgs.lua53Packages;
in {
  home.packages = [
    lua.lua
    lua.lua-lsp
  ];

  programs.nixvim = {
    plugins.lsp.enabledServers = ["lua_ls"];
  };

  programs.doom-emacs.config = {
    initModules = {
      lang = [
        {
          mod = "lua";
          args = ["lsp"];
        }
      ];
    };
  };
}
