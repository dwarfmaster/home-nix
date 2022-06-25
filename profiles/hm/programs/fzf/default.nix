{ pkgs, ... }:

let
  fd = pkgs.fd;
in {
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;

    changeDirWidgetCommand = "${fd}/bin/fd --type d";
    defaultCommand         = "${fd}/bin/fd --type f";
    fileWidgetCommand      = "${fd}/bin/fd --type f";
  };

  programs.broot = {
      enable = true;
      modal = true;
      enableZshIntegration = true;
  };
}

