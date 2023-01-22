{
  config,
  pkgs,
  lib,
  ...
}: let
  template = pkgs.fetchFromGitHub {
    owner = "aarowill";
    repo = "base16-alacritty";
    rev = "914727e48ebf3eab1574e23ca0db0ecd0e5fe9d0";
    sha256 = "sha256-oDsuiKx8gt+Ov7hZ9PibIQtE81IRSLO+n5N99WeiK34=";
  };
  colors = lib.mapAttrs' (name: v: lib.nameValuePair "${name}-hex" v) config.colorScheme.colors;
  values =
    colors
    // {
      scheme-name = config.colorScheme.slug;
      scheme-author = "???";
    };
  themeFile =
    config.lib.mustache.render
    "colors.yml"
    "${template}/templates/default-256.mustache"
    values;
in {
  programs.alacritty.enable = true;
  programs.alacritty.settings = {
    font = {
      size = 7.0; # For some reason alacritty double this number
      normal = {
        family = "FiraCode Nerd Font Mono";
        style = "Regular";
      };
    };
    import = [ themeFile ];
  };

  applications.terminal = "${config.programs.alacritty.package}/bin/alacritty";
}
