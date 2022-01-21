{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = with pkgs; [
    (agda.withPackages (p: [ p.standard-library ]))
  ];

  home.sessionVariables = {
    AGDA_DIR = "${config.xdg.configHome}/agda/";
  };
  xdg.configFile."agda/defaults".text = ''
    standard-library
  '';

  programs.doom-emacs.config = {
    initModules = {
      lang = [ { mod = "agda"; args = [ "local" ]; } ];
    };
  };
}
