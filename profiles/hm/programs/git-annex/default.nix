{
  config,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.git-annex
  ];
}
