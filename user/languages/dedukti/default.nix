{ config, lib, pkgs, ... }:

# I ressorted to maintain my own lambdapi-mode which is cloned from
# https://github.com/Deducteam/lambdapi, except that I don't start all features

{
  programs.doom = {
    modules.lang.dedukti = {
      config.source = ./config.el;
      packages.text = ''
        (package! lambdapi-mode)
      '';
      # autoloads = {
      #   mode.source = ./mode.el;
      # };
      # extras = {
      #   input.source = ./input.el;
      #   capf.source = ./capf.el;
      #   abbrev.source = ./abbrev.el;
      #   vars.source = ./vars.el;
      #   smie.source = ./smie.el;
      #   proofs.source = ./proofs.el;
      #   layout.source = ./layout.el;
      # };
    };
  };
}
