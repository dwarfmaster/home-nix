{ config, lib, pkgs, ... }:

{
  programs.alot = {
    enable = true;
    settings = {
      prefer_plaintext = true;
      initial_command = "search tag:inbox";
      handle_mouse = true;
      colourmode = 256;
      attachment_prefix = "/home/luc/downloads";
      compose_ask_tags = true;
      followup_to = true;
    };
  };

  programs.astroid = {
    enable = true;
    externalEditor = "${config.programs.emacs.finalPackage}/bin/emacsclient -q -c %1";
    #externalEditor = "st -f \"Monospace\" -w %3 -e vim %1";
    pollScript = "${pkgs.notmuch}/bin/notmuch new";
  };
}
