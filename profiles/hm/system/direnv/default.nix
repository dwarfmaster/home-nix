{ pkgs, ... }:

{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    nix-direnv.enableFlakes = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.doom-emacs.config = {
    initModules = {
      tools = [ "direnv" ];
    };
  };
}

