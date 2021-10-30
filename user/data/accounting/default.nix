{ config, lib, pkgs, ... }:

let

  influxdb-init = pkgs.writeScriptBin "hledger-influxdb-init" ''
    influx -password root -execute 'CREATE DATABASE finance'
  '';

in {
  home.packages = with pkgs; [
    haskellPackages.hledger
    influxdb-init
  ];

  programs.doom = {
    modules.dwarfmaster.hledger.config.source = ./hledger.el;
    initModules.lang = [ "ledger" ];
  };
}
