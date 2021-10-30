{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      pass # Unix password manager
      ;
  };

  programs.doom = {
    initModules = {
      tools = [ { mod = "pass"; args = [ "auth" ]; } ];
    };
  };
}
