{ term, config, ... }:

{
  enable   = true;
  cycle    = true;
  terminal = term;

  borderWidth = 2;
  theme       = "${config.xdg.configHome}/rofi/theme.rasi"; # TODO BAD !
  font        = "FuraCode Nerd Font Bold 20";
  fullscreen  = false;
  lines       = 15;
  location    = "center";
  scrollbar   = true;
  separator   = "solid";
  padding     = 5;

  extraConfig = builtins.readFile ./rofi/config;
}
