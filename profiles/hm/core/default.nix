{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./env
    ./shells
    ./hardware
  ];

  home.stateVersion = "21.11";
}
