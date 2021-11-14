{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  programs.cookiecutter = {
    enable = true;
    defaults = {
      full_name = "Luc Chabassier";
      email = "luc@dwarfmaster.net";
      github_username = "dwarfmaster";
      cpp_standard = "20";
    };
  };

  # TODO move to home.shellAliases when it is available
  programs.zsh.shellAliases = {
    tp = "${config.programs.cookiecutter.package}/bin/cookiecutter";
  };
}
