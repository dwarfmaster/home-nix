{
  config,
  pkgs,
  ...
}: let
  yuck-vim = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "yuck.vim";
    version = "9b5e0370f70cc30383e1dabd6c215475915fe5c3";
    src = pkgs.fetchFromGitHub {
      owner = "elkowar";
      repo = "yuck.vim";
      rev = "9b5e0370f70cc30383e1dabd6c215475915fe5c3";
      sha256 = "1mkf0vd8vvw1njlczlgai80djw1n1a7dl1k940l089d3vvqr5dhp";
    };
  };

  colors = config.colorScheme.colors;
  eww-config = config.lib.mustache.renderDir "eww" ./status_bar colors;
in {
  programs.eww = {
    enable = true;
    configDir = eww-config;
  };
  xsession.windowManager.bspwm.extraConfig = ''
    ${config.programs.eww.package}/bin/eww open-many \
        statusbar-left \
        statusbar-center \
        statusbar-right
  '';

  programs.nixvim = {
    extraPlugins = [yuck-vim];
  };
}
