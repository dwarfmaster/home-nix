general@{ recdata, ... }:

let
  pkgs = general.pkgs.main;
  fd   = pkgs.fd;
in {
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;

    changeDirWidgetCommand = "${fd}/bin/fd --type d";
    defaultCommand         = "${fd}/bin/fd --type f";
    fileWidgetCommand      = "${fd}/bin/fd --type f";
    historyWidgetCommand   = "history 0";
  };
}

