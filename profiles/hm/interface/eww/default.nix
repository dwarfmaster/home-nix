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
in {
  programs.eww = {
    enable = false;
    configDir = ./status_bar;
  };
  home.packages = [pkgs.eww];

  programs.nixvim = {
    extraPlugins = [yuck-vim];
  };
}
