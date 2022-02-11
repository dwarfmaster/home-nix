{ lib, homeManagerConfiguration, finalHMModules, pkgs, ... }:

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
        ];
      };
      system = "x86_64-linux";
      homeDirectory = "/home/${config.username}";
      username = config.username;
      extraModules = builtins.attrValues finalHMModules;
      pkgs = pkgs;
      stateVersion = "21.11";
    };

in builtins.mapAttrs mkHm configurations
