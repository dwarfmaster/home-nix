{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  imports = [ ./hm.nix ];

  programs.cookiecutter = {
    enable = true;
    extraConfig = {
      default_context = {
        full_name = "Luc Chabassier";
        email = "luc@dwarfmaster.net";
        github_username = "dwarfmaster";
        cpp_standard = "11";
      };
      cookiecutters_dir = "${config.xdg.stateHome}/cookiecutter/cookies";
      replay_dir = "${config.xdg.stateHome}/cookiecutter/replays";
    };
  };
}
