{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  imports = [ ./hm.nix ];

  programs.cookiecutter = {
    enable = true;
    defaults = {
      full_name = "Luc Chabassier";
      email = "luc@dwarfmaster.net";
      github_username = "dwarfmaster";
      cpp_standard = "20";
    };
    templates = import ./templates.nix { inherit config lib; };
  };
}
