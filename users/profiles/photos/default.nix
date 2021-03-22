{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    darktable   # Image manager
    geeqie      # Image manager
    sxiv        # Lightweight X image viewer
    imagemagick # Convert any image format to any other
  ];

  xdg.configFile."darktable/luarc".source = ./darktable.lua;
}
