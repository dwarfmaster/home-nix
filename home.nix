{ ... }:

let

  general = import ./default.nix { };

  configs = import ./home/home.nix general;

in configs.main

