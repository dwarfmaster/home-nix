{ lib, homeManagerConfiguration, finalHMModules, pkgs, ... }:

let

  mkConfig = path:
    { imports = [ path ../config/core ] ++ builtins.attrValues finalHMModules; };

  configurations = {
    luc        = {
      username = "luc";
      config   = mkConfig ./luc.nix;
    };
    luc-server = {
      username = "luc";
      config   = mkConfig ./luc-server.nix;
    };
  };

  mkHm = name: config:
    homeManagerConfiguration {
      configuration = config.config;
      system = "x86_64-linux";
      homeDirectory = "/home/${config.username}";
      username = config.username;
      pkgs = pkgs;
      stateVersion = "21.11";
    };

  activations = builtins.mapAttrs mkHm configurations;

in activations // { configurations = builtins.mapAttrs (_: config: config.config) configurations; }
