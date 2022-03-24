{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkOption types
    mkIf concatStringsSep;
  cfg = config.programs.coq;
in {
  options = {
    programs.coq = {
      enable = mkEnableOption "Install coq program";
      packages = mkOption {
        description = "The coq package set";
        type = types.attrs;
        default = pkgs.coqPackages;
      };
      package = mkOption {
        description = "The coq package to use";
        type = types.package;
        default = cfg.packages.coq;
      };
      libraries = mkOption {
        description = "Coq libraries to install globally";
        type = types.listOf types.package;
        default = [ ];
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ] ++ cfg.libraries;
    home.sessionVariables = {
      COQPATH = concatStringsSep ":" (map (lib: "${lib}/lib/coq/${cfg.package.coq-version}/user-contrib") cfg.libraries);
      OCAMLPATH = "${cfg.package}/lib/ocaml/${cfg.package.ocamlPackages.ocaml.version}/site-lib";
    };
  };
}
