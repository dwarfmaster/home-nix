{
  config,
  pkgs,
  lib,
  ...
}: {
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
  plugins.telescope.enabledExtensions = ["notify"];
  maps.normal = {
    "<leader>sN" = {
      action = "<cmd>Telescope notify<cr>";
      desc = "Notifications";
    };
  };
}
