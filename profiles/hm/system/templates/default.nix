{ config, lib, pkgs, ... }:

{
  programs.cookiecutter = {
    enable = true;
    defaults = {
      full_name = "Luc Chabassier";
      email = "luc@dwarfmaster.net";
      github_username = "dwarfmaster";
      cpp_standard = "20";
      nixpkgs_version = "release-21.05";
      # TODO make it machine dependent
      build_cores = "12";
    };
  };

  # TODO move to home.shellAliases when it is available
  programs.zsh.shellAliases = {
    tp = "${config.programs.cookiecutter.package}/bin/cookiecutter";
  };
}
