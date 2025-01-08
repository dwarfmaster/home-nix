{
  config,
  lib,
  pkgs,
  ...
}: {
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

  # TODO choose whether to fix or remove
  # programs.astroid = {
  #   enable = false;
  #   externalEditor = "${config.programs.kitty.package}/bin/kitty -e nvim %1";
  #   pollScript = "${pkgs.notmuch}/bin/notmuch new";
  # };
  #
  # programs.nixvim.autoCmd = [
  #   {
  #     event = ["BufNewFile" "BufRead"];
  #     pattern = ["${config.xdg.cacheHome}/astroid/*"];
  #     command = "set ft=mail";
  #   }
  # ];
}
