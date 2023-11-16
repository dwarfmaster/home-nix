{
  pkgs,
  ...
}: {
  home.packages = [ pkgs.idris2 ];
  programs.doom-emacs.config.initModules.lang = ["idris"];
}
