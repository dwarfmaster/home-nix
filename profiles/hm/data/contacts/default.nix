{
  config,
  pkgs,
  ...
}: {
  home.packages = [pkgs.khard];
  xdg.configFile."khard/khard.conf".source = ./khard.conf;
}
