{ lib, utils, homeManagerConfiguration, finalHMModules, pkgset, ... }:

let

  configurations = {
    luc        = {
      username = "luc";
      config   = import ./luc.nix;
    };
    luc-server = {
      username = "luc";
      config   = import ./luc-server.nix;
    };
  };

  mkHm = name: config:
    homeManagerConfiguration {
      configuration = {
        imports = [
          config.config
          ../config/core
          (utils.mkPackagesModule pkgset)
        ];
      };
      system = "x86_64-linux";
      homeDirectory = "/home/${config.username}";
      username = config.username;
      extraModules = builtins.attrValues finalHMModules;
      pkgs = pkgset.pkgs;
      stateVersion = "21.11";
    };

in builtins.mapAttrs mkHm configurations
