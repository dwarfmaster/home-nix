{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    haskellPackages.hledger
  ];
}
