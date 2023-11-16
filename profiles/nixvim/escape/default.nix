{
  config,
  pkgs,
  lib,
  ...
}: {
  plugins.easyescape = {
    enable = true;
  };
  globals = {
    easyescape_chars = {
      "j" = 1;
      "k" = 1;
    };
    easyescape_timeout = 100;
  };
  # maps.insert = {
  #   "jk".desc = "Exits insert mode";
  #   "kj".desc = "Exits insert mode";
  # };
  plugins.which-key.triggersBlackList.i = ["j" "k"];
}
