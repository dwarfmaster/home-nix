{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    calibre
    okular # Heavy full featured pdf reader
  ];

  home.sessionVariables = {
    CALIBRE_CONFIG_DIRECTORY = "${config.xdg.configHome}/calibre";
    CALIBRE_CACHE_DIRECTORY = "${config.xdg.cacheHome}/calibre/cache";
    CALIBRE_USE_SYSTEM_THEME = "yes";
  };
}
