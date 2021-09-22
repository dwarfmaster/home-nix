{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    promptInit = ''
      eval "$(${pkgs.starship}/bin/starship init zsh)"
    '';
    shellInit = ''
      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
    '';
  };

  # System completion for ZSH
  environment.pathsToLink = [ "/share/zsh" ];
}
