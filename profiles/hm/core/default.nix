{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./env
    ./shells
  ];

  home.stateVersion = "21.11";
}
