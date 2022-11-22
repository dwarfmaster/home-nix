{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [pkgs.lean];
  programs.doom-emacs.config.initModules.lang = ["lean"];
}
