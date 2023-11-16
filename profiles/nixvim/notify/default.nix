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
  keymaps = [
    {
      key = "<leader>sN";
      action = "<cmd>Telescope notify<cr>";
      options.desc = "Notifications";
    }
  ];
}
