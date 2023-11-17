{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [
    (pkgs.agda.withPackages (p: [p.standard-library]))
  ];

  home.sessionVariables = {
    AGDA_DIR = "${config.xdg.configHome}/agda/";
  };
  xdg.configFile."agda/defaults".text = ''
    standard-library
  '';
}
