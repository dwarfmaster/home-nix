{ config, lib, pkgs, ... }:

{
  programs.bash = {
    shellInit = ''
      eval "$(${pkgs.direnv}/bin/direnv hook bash)"
    '';
  };
}
