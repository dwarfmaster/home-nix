{ config, lib, pkgs, ... }:

let
  lua = pkgs.lua53Packages;
in {
  home.packages = [
    lua.lua
  ];

  programs.doom-emacs.config = {
    initModules = {
      lang = [ { mod = "lua"; args = [ "lsp" ]; } ];
    };
    # TODO derivation for sumneko-lsp hardcode ~/.cache for logs and meta
    modules.dwarfmaster.lua = {
      config.text = ''
        (after! lsp-lua
          (setq lsp-clients-lua-language-server-main-location "${pkgs.sumneko-lua-language-server}/share/lua-language-server/main.lua")
          (setq lsp-clients-lua-language-server-bin "${pkgs.sumneko-lua-language-server}/bin/lua-language-server"))
      '';
    };
  };
}
