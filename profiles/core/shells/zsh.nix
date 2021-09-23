{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    shellInit = ''
      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
    '';
  };

  # System completion for ZSH
  environment.pathsToLink = [ "/share/zsh" ];
}
