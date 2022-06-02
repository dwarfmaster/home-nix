{ config, lib, pkgs, ... }:

{
  programs.cheat = {
    enable = true;
    enableCommunity = true;
    extraConfig = {
      editor = "vim";
      colorize = true;
      style = "autumn";
      formatter = "terminal256";
      pager = "${pkgs.less}/bin/less -FRX";
    };
  };
}
