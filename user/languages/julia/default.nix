{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
  fhsCommand = pkgs.callPackage pkgs.scientific-fhs {
    juliaVersion = "julia_16";
    condaInstallationPath = "${config.xdg.cacheHome}/conda";
    enableConda = false; # I don't need conda for now
  };
in {
  home.packages = [
    (fhsCommand "julia" "julia")
    (fhsCommand "julia-bash" "bash")
  ];

  home.sessionVariables = {
    JULIA_DEPOT_PATH = "${config.xdg.cacheHome}/julia";
  };

  programs.doom-emacs.config = {
    initModules = {
      lang = [ { mod = "julia"; args = [ "lsp" ]; } ];
    };
  };
}
