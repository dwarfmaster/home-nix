{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      pass # Unix password manager
      ;
  };

  programs.doom-emacs.config = {
    initModules = {
      tools = [ { mod = "pass"; args = [ ]; } ];
    };
  };
}
