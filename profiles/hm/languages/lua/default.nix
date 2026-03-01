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
    lsp.servers.lua_ls.enable = true;
  };
}
