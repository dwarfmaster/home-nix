{ pkgs, ... } @ args:

let content = import ./main.nix; in

let lib = import ./lib.nix; in

(lib.fix content) args

