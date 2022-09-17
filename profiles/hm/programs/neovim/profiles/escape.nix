{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    plugins.easyescape = {
      enable = true;
    };
    globals = {
      easyescape_chars = { "j" = 1; "k" = 1; };
      easyescape_timeout = 100;
    };
    plugins.which-key = {
      bindings.i = {
        "j".bindings."k" = { binding = "<esc>"; description = "Exits insert mode"; };
        "k".bindings."j" = { binding = "<esc>"; description = "Exits insert mode"; };
      };
      triggers.blacklist.i = [ "j" "k" ];
    };
  };
}
