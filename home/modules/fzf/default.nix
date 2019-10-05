{ pkgs, recdata, ... }:

{
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;

    changeDirWidgetCommand = "fd --type d";
    defaultCommand         = "fd --type f";
    fileWidgetCommand      = "fd --type f";
    historyWidgetCommand   = "history 0";
  };

  packages = [ pkgs.fd ];
}

