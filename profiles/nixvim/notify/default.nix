{
  config,
  pkgs,
  lib,
  ...
}: let
  desktop-notify = pkgs.vimUtils.buildVimPlugin {
    name = "desktop-notify.nvim";
    src = pkgs.fetchFromGitLab {
      owner = "HiPhish";
      repo = "desktop-notify.nvim";
      rev = "e1e684226d9b4a7313439bc7dd1be09d72bfb839";
      sha256 = "sha256-cT5XxqGF3RNpQiVn0MXZUFd0PMnBPcE7ioegfqCiUnM=";
    };
  };
in {
  extraPlugins = [
    desktop-notify
  ];
  extraConfigLua = ''
    vim.notify = require('desktop_notify').notify_send
  '';
}
