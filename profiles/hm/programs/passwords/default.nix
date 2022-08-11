{ config, pkgs, ... }:

{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      pass # Unix password manager
      keepassxc # An advanced passphrase manager
      ;
  };

  programs.doom-emacs.config = {
    initModules = {
      tools = [ { mod = "pass"; args = [ ]; } ];
    };
  };
}
