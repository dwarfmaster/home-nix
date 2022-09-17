{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    plugins.notify = {
      enable = true;
      stages = "slide";
      # TODO
      # icons = {
      #   error = "";
      #   warn = "";
      #   info = "";
      #   debug = "";
      #   trace = "";
      # };
    };
    plugins.telescope.enabledExtensions = [ "notify" ];
    plugins.which-key.bindings.n."<leader>".subs."s".bindings = {
      "N" = { cmd = "Telescope notify"; description = "Notifications"; };
    };
  };
}
