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
}
